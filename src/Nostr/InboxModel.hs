{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nostr.InboxModel where

import Control.Monad (forever, forM, forM_, unless, void, when)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, cancel,forConcurrently)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, put, modify)
import Effectful.TH

import Logging
import Nostr
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.RelayConnection (RelayConnection, connect, disconnect)
import Nostr.Subscription 
    ( Subscription
    , giftWrapFilter
    , handleEvent
    , mentionsFilter
    , profilesFilter
    , stopAllSubscriptions
    , stopSubscription
    , subscribe
    , userPostsFilter
    , commentsFilter
    )
import Nostr.Types
  ( RelayURI
  , Relay(..)
  , Event(..)
  , EventId
  , Filter(..)
  , Kind(..)
  , SubscriptionId
  , getUri
  , isInboxCapable
  , isOutboxCapable
  , defaultGeneralRelays
  )
import Nostr.Util
import QtQuick (QtQuick, UIUpdates(..), notify)
import RelayMgmt
import Store.Lmdb (LmdbStore, getFollows, getGeneralRelays, getDMRelays, getLatestTimestamp)
import Types (AppState(..), ConnectionState(..), Follow(..), SubscriptionDetails(..), RelayPool(..), RelayData(..), SubscriptionEvent(..), initialRelayPool)

-- | InboxModel effects
data InboxModel :: Effect where
  StartInboxModel :: InboxModel m ()
  StopInboxModel :: InboxModel m ()
  AwaitAtLeastOneConnected :: InboxModel m Bool
  SubscribeToCommentsFor :: EventId -> InboxModel m ()
  UnsubscribeToCommentsFor :: EventId -> InboxModel m ()

type instance DispatchOf InboxModel = Dynamic

makeEffect ''InboxModel

type InboxModelEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
  , Subscription :> es
  , RelayConnection :> es
  , RelayMgmt :> es
  , Logging :> es
  , Concurrent :> es
  , Util :> es
  , Nostr :> es
  , QtQuick :> es
  )

-- | Run InboxModel
runInboxModel :: InboxModelEff es => Eff (InboxModel : es) a -> Eff es a
runInboxModel = interpret $ \_ -> \case
  StartInboxModel -> do
    kp <- getKeyPair
    let xo = keyPairToPubKeyXO kp
    queue <- newTQueueIO
    updateQueue' <- newTQueueIO
    
    -- Start the event processing loop
    void $ async $ do
      initializeSubscriptions xo
      eventLoop xo

    -- Start and track the batched update loop
    updateThread' <- async $ forever $ do
      void $ atomically $ readTQueue updateQueue'
      void $ atomically $ flushTQueue updateQueue'
      logInfo "Processing batched metadata updates."
      updateSubscriptions xo
      threadDelay 20000000  -- Wait 20 seconds
    
    modify @RelayPool (\s -> s { inboxQueue = queue
                               , updateQueue = updateQueue'
                               , updateThread = Just updateThread'
                               })

  StopInboxModel -> do
    st <- get @RelayPool

    forM_ (updateThread st) cancel

    modify @RelayPool $ \s -> s { updateThread = Nothing }

    forM_ (Map.keys $ activeConnections st) $ \relayUri -> do
      disconnect relayUri

    put @RelayPool initialRelayPool

  AwaitAtLeastOneConnected -> awaitAtLeastOneConnected'

  SubscribeToCommentsFor eid -> do
    pool <- get @RelayPool
    queue <- gets @RelayPool inboxQueue
    let activeRelays = Map.keys $ activeConnections pool

    subIds <- forM activeRelays $ \relayUri -> do
      subscribe relayUri (commentsFilter eid) queue

    modify @RelayPool $ \s ->
      s { commentSubscriptions = Map.insert eid subIds (commentSubscriptions s) }

  UnsubscribeToCommentsFor eid -> do
    pool <- get @RelayPool
    let subIds = Map.findWithDefault [] eid (commentSubscriptions pool)

    forM_ subIds stopSubscription

    modify @RelayPool $ \s ->
      s { commentSubscriptions = Map.delete eid (commentSubscriptions pool) }

-- | Wait until at least one relay is connected
awaitAtLeastOneConnected' :: InboxModelEff es => Eff es Bool
awaitAtLeastOneConnected' = do
  let loop = do
        st <- get @RelayPool
        let states = map (connectionState . snd) $ Map.toList $ activeConnections st
        if any (== Connected) states
            then return True
            else if null states
              then do
                threadDelay 50000  -- 50ms delay
                loop
              else if all (== Disconnected) states
                  then return False
                  else do
                      threadDelay 50000  -- 50ms delay
                      loop
  loop

-- | Initialize subscriptions
initializeSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
initializeSubscriptions xo = do
  inboxRelays <- getGeneralRelays xo

  if null inboxRelays
    then initializeWithDefaultRelays xo
    else continueWithRelays inboxRelays

-- | Initialize using default relays when no relay configuration exists
initializeWithDefaultRelays :: InboxModelEff es => PubKeyXO -> Eff es ()
initializeWithDefaultRelays xo = do
  logDebug "Initializing with default relays..."
  let (defaultRelays, _) = defaultGeneralRelays

  connectionResults <- forConcurrently defaultRelays $ \r -> do
    connected <- connect (getUri r)
    return (r, connected)

  let connectedRelays = [r | (r, success) <- connectionResults, success]

  initQueue <- newTQueueIO
  let filter' = profilesFilter [xo] Nothing

  subIds <- subscribeToFilter connectedRelays filter' initQueue
  receivedEvents <- collectEventsUntilEose initQueue

  forM_ receivedEvents $ \(r, e') -> do
    case e' of
      EventAppeared e'' -> do
        updates <- handleEvent r e''
        notify updates
      _ -> return ()

  forM_ subIds stopSubscription

  follows <- getFollows xo
  let followList = map pubkey follows

  unless (null followList) $ do
    let filter'' = profilesFilter followList Nothing

    subIds' <- subscribeToFilter connectedRelays filter'' initQueue
    receivedEvents' <- collectEventsUntilEose initQueue

    forM_ receivedEvents' $ \(r, e') -> do
      case e' of
        EventAppeared e'' -> do
          updates <- handleEvent r e''
          notify updates
        _ -> return ()

    forM_ subIds' stopSubscription

  let hasRelayMeta = hasRelayListMetadata $ map snd receivedEvents
      hasPreferredDM = hasPreferredDMRelays $ map snd receivedEvents

  unless hasRelayMeta $ do
    logInfo "RelayListMetadata event not found. Setting default general relays."
    setDefaultGeneralRelays xo

  unless hasPreferredDM $ do
    logInfo "PreferredDMRelays event not found. Setting default DM relays."
    setDefaultDMRelays xo

  -- Continue with appropriate relays
  inboxRelays <- getGeneralRelays xo
  continueWithRelays inboxRelays


-- | Subscribe to a filter on multiple relays
subscribeToFilter
  :: InboxModelEff es
  => [Relay]
  -> Filter
  -> TQueue (RelayURI, SubscriptionEvent)
  -> Eff es [SubscriptionId]
subscribeToFilter relays f queue = do
    forM relays $ \r -> do
        let relayUri = getUri r
        subscribe relayUri f queue

-- | Collect events until EOSE is received from all subscriptions
collectEventsUntilEose :: InboxModelEff es => TQueue (RelayURI, SubscriptionEvent) -> Eff es [(RelayURI, SubscriptionEvent)]
collectEventsUntilEose queue = do
    st <- get @RelayPool
    let expectedEoseCount = length $ Map.keys $ activeConnections st

    let loop acc eoseRelays = do
          event@(relayUri, evt) <- atomically $ readTQueue queue
          case evt of
            SubscriptionEose ->
              let newEoseRelays = Set.insert relayUri eoseRelays
              in if Set.size newEoseRelays == expectedEoseCount  -- All active relays have sent EOSE
                 then return (event : acc)
                 else loop (event : acc) newEoseRelays
            SubscriptionClosed _ -> loop acc eoseRelays
            _ -> loop (event : acc) eoseRelays

    if expectedEoseCount == 0
      then return []
      else loop [] Set.empty

-- | Check if RelayListMetadata event is present
hasRelayListMetadata :: [SubscriptionEvent] -> Bool
hasRelayListMetadata events = any isRelayListMetadata events
  where
    isRelayListMetadata (EventAppeared event') = kind event' == RelayListMetadata
    isRelayListMetadata _ = False

-- | Check if PreferredDMRelays event is present
hasPreferredDMRelays :: [SubscriptionEvent] -> Bool
hasPreferredDMRelays events = any isPreferredDMRelays events
  where
    isPreferredDMRelays (EventAppeared event') = kind event' == PreferredDMRelays
    isPreferredDMRelays _ = False

-- | Continue with discovered relays
continueWithRelays :: InboxModelEff es => [Relay] -> Eff es ()
continueWithRelays inboxRelays = do
  kp <- getKeyPair
  let xo = keyPairToPubKeyXO kp
  let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]

  dmRelays <- getDMRelays xo

  logDebug $ "Initializing subscriptions for Discovered Inbox Relays: " <> pack (show ownInboxRelayURIs)
  logDebug $ "Initializing subscriptions for Discovered DM Relays: " <> pack (show dmRelays)

  -- Connect to DM relays concurrently
  void $ forConcurrently dmRelays $ \r -> do
    connected <- connect r
    when connected $ subscribeToGiftwraps r xo

  -- Connect to inbox relays concurrently
  void $ forConcurrently inboxRelays $ \r -> do
    let relayUri = getUri r
    connected <- connect relayUri
    when (connected && isInboxCapable r) $ do
      subscribeToMentions relayUri xo
      subscribeToProfilesAndPosts relayUri [xo]

  follows <- getFollows xo
  let followList = xo : map pubkey follows
  followRelayMap <- buildRelayPubkeyMap followList ownInboxRelayURIs
  --logDebug $ "Build Relay-PubKey Map: " <> pack (show followRelayMap)

  -- Connect to follow relays concurrently
  void $ forConcurrently (Map.toList followRelayMap) $ \(relayUri, pubkeys) -> do
    connected <- connect relayUri
    when connected $ subscribeToProfilesAndPosts relayUri pubkeys

-- | Subscribe to Giftwrap events on a relay
subscribeToGiftwraps :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToGiftwraps relayUri xo = do
  lastTimestamp <- getSubscriptionTimestamp [xo] [GiftWrap]
  let lastTimestamp' = fmap (\ts -> ts - (2 * 24 * 60 * 60)) lastTimestamp  -- 2 days in seconds
  queue <- gets @RelayPool inboxQueue
  void $ subscribe relayUri (giftWrapFilter xo lastTimestamp') queue

-- | Subscribe to mentions on a relay
subscribeToMentions :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToMentions relayUri xo = do
  lastTimestamp <- getSubscriptionTimestamp [xo] [ShortTextNote, Repost, Comment, EventDeletion]
  queue <- gets @RelayPool inboxQueue
  void $ subscribe relayUri (mentionsFilter xo lastTimestamp) queue


-- | Subscribe to profiles and posts for a relay
subscribeToProfilesAndPosts :: InboxModelEff es => RelayURI -> [PubKeyXO] -> Eff es ()
subscribeToProfilesAndPosts relayUri pks = do
  -- Subscribe to profiles
  queue <- gets @RelayPool inboxQueue
  void $ subscribe relayUri (profilesFilter pks Nothing) queue

  -- Subscribe to posts
  postsLastTimestamp <- getSubscriptionTimestamp pks [ShortTextNote, Repost, EventDeletion]
  void $ subscribe relayUri (userPostsFilter pks postsLastTimestamp) queue


getSubscriptionTimestamp :: InboxModelEff es => [PubKeyXO] -> [Kind] -> Eff es (Maybe Int)
getSubscriptionTimestamp pks ks = do
  timestamps <- forM pks $ \pk -> getLatestTimestamp pk ks
  if null timestamps 
    then return Nothing
    else case catMaybes timestamps of
      [] -> return Nothing
      ts -> return $ Just $ minimum ts

-- | Build a map from relay URI to pubkeys, prioritizing existing inbox relays
buildRelayPubkeyMap :: InboxModelEff es => [PubKeyXO] -> [RelayURI] -> Eff es (Map.Map RelayURI [PubKeyXO])
buildRelayPubkeyMap pks ownInboxRelays = do
  relayPubkeyPairs <- forM pks $ \pk -> do
    relays <- getGeneralRelays pk

    let outboxRelayURIs = [ getUri r | r <- relays, isOutboxCapable r ]

        prioritized = filter (`elem` ownInboxRelays) outboxRelayURIs

        nonPrioritized = filter (not . (`elem` ownInboxRelays)) outboxRelayURIs

        selectedRelays
          | null outboxRelayURIs = ownInboxRelays
          | not (null prioritized) && not (null nonPrioritized) =
              take maxRelaysPerContact (take 1 prioritized ++ nonPrioritized)
          | not (null prioritized) = take maxRelaysPerContact prioritized
          | otherwise = take maxRelaysPerContact nonPrioritized

    return (pk, selectedRelays)

  return $ Map.filter (not . null) $ foldr (\(pk, relays) acc ->
    foldr (\r acc' -> Map.insertWith (++) r [pk] acc') acc relays
    ) Map.empty relayPubkeyPairs

-- | Maximum number of outbox relays to consider per contact
maxRelaysPerContact :: Int
maxRelaysPerContact = 2

-- | Event loop to handle incoming events and updates
eventLoop :: InboxModelEff es => PubKeyXO -> Eff es ()
eventLoop xo = do
  shouldStopVar <- newTVarIO False

  let loop = do
        events <- collectEvents

        forM_ events $ \(relayUri, event) -> do
          case event of
            EventAppeared event' -> do
              updates <- handleEvent relayUri event'
              when (myFollowsChanged updates || dmRelaysChanged updates || generalRelaysChanged updates) $ do
                updateSubscriptions xo
              notify updates
            SubscriptionEose -> return ()
            SubscriptionClosed _ -> atomically $ writeTVar shouldStopVar True

        shouldStop <- atomically $ readTVar shouldStopVar
        unless shouldStop loop
  loop

-- | Collect events from all active subscriptions
collectEvents :: InboxModelEff es => Eff es [(RelayURI, SubscriptionEvent)]
collectEvents = do
  queue <- gets @RelayPool inboxQueue
  atomically $ do
    event <- readTQueue queue
    remainingEvents <- flushTQueue queue
    return (event : remainingEvents)

-- | Update subscriptions when follows or preferred DM relays change
updateSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateSubscriptions xo = do
  updateGeneralSubscriptions xo
  updateDMSubscriptions xo

-- | Update general subscriptions based on current follow list
updateGeneralSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateGeneralSubscriptions xo = do
  follows <- getFollows xo
  let followList = map pubkey follows

  inboxRelays <- getGeneralRelays xo
  let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]

  void $ forConcurrently inboxRelays $ \relay' -> do
    when (isInboxCapable relay') $ do
      let relayUri = getUri relay'
      connected <- connect relayUri
      when connected $ subscribeToMentions relayUri xo

  newRelayPubkeyMap <- buildRelayPubkeyMap followList ownInboxRelayURIs

  pool <- get @RelayPool
  let currentRelays = Map.keysSet (activeConnections pool)
  let newRelays = Map.keysSet newRelayPubkeyMap

  let relaysToAdd = Set.difference newRelays currentRelays
  let relaysToRemove = Set.difference currentRelays newRelays
  let relaysToUpdate = Set.intersection currentRelays newRelays

  void $ forConcurrently (Set.toList relaysToRemove) $ \relayUri -> do
    disconnect relayUri

  void $ forConcurrently (Set.toList relaysToAdd) $ \relayUri -> do
    let pubkeys = Map.findWithDefault [] relayUri newRelayPubkeyMap
    connected <- connect relayUri
    when connected $ do
      subscribeToProfilesAndPosts relayUri pubkeys

  void $ forConcurrently (Set.toList relaysToUpdate) $ \relayUri -> do
    let newPubkeys = Set.fromList $ Map.findWithDefault [] relayUri newRelayPubkeyMap
    let currentPubkeys = Set.fromList $ getSubscribedPubkeys pool relayUri
    when (newPubkeys /= currentPubkeys) $ do
      stopAllSubscriptions relayUri
      subscribeToProfilesAndPosts relayUri (Set.toList newPubkeys)

-- | Update DM subscriptions
updateDMSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateDMSubscriptions xo = do
    kp <- getKeyPair
    let ownPubkey = keyPairToPubKeyXO kp

    when (xo == ownPubkey) $ do
        dmRelayURIs <- getDMRelays xo
        let dmRelaySet = Set.fromList dmRelayURIs
        pool <- get @RelayPool

        let giftwrapSubs =
              [ (relayUri, subId)
              | (subId, sd) <- Map.toList (subscriptions pool)
              , GiftWrap `elem` fromMaybe [] (kinds $ subscriptionFilter sd)
              , relayUri <- Map.keys (activeConnections pool)
              , not (relayUri `Set.member` dmRelaySet)
              ]

        -- Stop giftwrap subscriptions for relays not in dmRelaySet
        void $ forConcurrently giftwrapSubs $ \(relayUri, subId) -> do
            stopSubscription subId
            let hasOtherSubs = any (\_ -> relayUri == relayUri)
                                  (Map.elems $ subscriptions pool)
            when (not hasOtherSubs) $ do
                disconnect relayUri

        let currentRelaySet = Map.keysSet (activeConnections pool)
        let relaysToAdd = Set.difference dmRelaySet currentRelaySet

        void $ forConcurrently (Set.toList relaysToAdd) $ \relayUri -> do
            connected <- connect relayUri
            when connected $ do
                subscribeToGiftwraps relayUri xo

-- | Get RelayData for a relay
getRelayData :: InboxModelEff es => RelayURI -> Eff es RelayData
getRelayData relayUri = do
  st <- get @RelayPool
  return $ fromMaybe (error $ "No RelayData for " <> unpack relayUri) $ Map.lookup relayUri (activeConnections st)

-- | Get pubkeys from subscriptions in RelayData
getSubscribedPubkeys :: RelayPool -> RelayURI -> [PubKeyXO]
getSubscribedPubkeys pool relayUri =
  [ pk
  | (_, sd) <- Map.toList (subscriptions pool)
  , relay sd == relayUri
  , pk <- fromMaybe [] (authors $ subscriptionFilter sd)
  ]
