{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Nostr.InboxModel where

import Control.Monad (forever, forM, forM_, unless, void, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, cancel, forConcurrently)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, put, modify)
import Effectful.TH
import Data.List (maximumBy)
import Data.Ord (comparing)

import Logging
import Nostr
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.RelayConnection (RelayConnection, connect, disconnect)
import Nostr.Subscription 
    ( Subscription
    , SubscriptionEff
    , giftWrapFilter
    , mentionsFilter
    , profilesFilter
    , stopAllSubscriptions
    , stopSubscription
    , subscribe
    , userPostsFilter
    , commentsFilter
    )
import Nostr.SubscriptionHandler
import Nostr.Types
  ( RelayURI
  , Relay(..)
  , Event(..)
  , EventId
  , Filter(..)
  , Kind(..)
  , SubscriptionId
  , emptyFilter
  , getUri
  , isInboxCapable
  , isOutboxCapable
  , defaultGeneralRelays
  )
import Nostr.Util
import QtQuick (QtQuick, UIUpdates(..), notify)
import RelayMgmt
import Store.Lmdb ( LmdbStore, getFollows, getGeneralRelays, getDMRelays
                  , getLatestTimestamp, getFailedRelaysWithinLastNDays, recordFailedRelay )
import Types ( AppState(..), ConnectionState(..), Follow(..), SubscriptionState(..)
             , RelayPool(..), RelayData(..), SubscriptionEvent(..), initialRelayPool )

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
  , SubscriptionHandler :> es
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
    xo <- keyPairToPubKeyXO <$> getKeyPair
    inboxRelays <- getGeneralRelays xo
    updateQueue' <- newTQueueIO

    -- Start and track the batched update loop
    updateThread' <- async $ forever $ do
      void $ atomically $ readTQueue updateQueue'
      void $ atomically $ flushTQueue updateQueue'
      logInfo "Processing batched metadata updates."
      updateSubscriptions xo
      threadDelay 10000000  -- Relay updates are processed every 10 seconds
    
    modify @RelayPool $ \s -> s
        { updateQueue = updateQueue'
        , updateThread = Just updateThread'
        }
    

    if null inboxRelays
      then initializeWithDefaultRelays xo
      else continueWithRelays inboxRelays

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
    let activeRelays = Map.keys $ activeConnections pool

    forM_ activeRelays $ \relayUri -> do
      queue <- newTQueueIO
      subId' <- subscribe relayUri (commentsFilter eid) queue
      modify @RelayPool $ \s -> s { commentSubscriptions = Map.insertWith (++) eid [subId'] (commentSubscriptions s) }
      void $ async $ handleSubscription subId' queue

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

-- | Initialize using default relays when no relay configuration exists
initializeWithDefaultRelays :: InboxModelEff es => PubKeyXO -> Eff es ()
initializeWithDefaultRelays xo = do
  logDebug "Initializing with default relays..."
  let (defaultRelays, _) = defaultGeneralRelays

  connectionResults <- forConcurrently defaultRelays $ \r -> do
    connected <- connect (getUri r)
    return (r, connected)

  let connectedRelays = [r | (r, success) <- connectionResults, success]
  let bootstrapFilter = inboxRelayTopologyFilter [xo]

  void $ forConcurrently connectedRelays $ \r -> do
      queue <- newTQueueIO
      subId' <- subscribe (getUri r) bootstrapFilter queue
      void $ async $ handleSubscription subId' queue

  myGeneralRelays <- getGeneralRelays xo
  myDMRelays <- getDMRelays xo

  when (null myGeneralRelays) setDefaultGeneralRelays
  when (null myDMRelays) setDefaultDMRelays

  inboxRelays <- getGeneralRelays xo
  continueWithRelays inboxRelays


-- | Continue with discovered relays
continueWithRelays :: InboxModelEff es => [Relay] -> Eff es ()
continueWithRelays inboxRelays = do
  kp <- getKeyPair
  let xo = keyPairToPubKeyXO kp
  let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]

  dmRelays <- getDMRelays xo
  follows <- getFollows xo
  let followList = xo : map pubkey follows

  relayMap <- getRelayListsForPubkeys followList

  logDebug $ "Initializing subscriptions for Discovered Inbox Relays: " <> pack (show ownInboxRelayURIs)
  logDebug $ "Initializing subscriptions for Discovered DM Relays: " <> pack (show dmRelays)

  -- Connect to DM relays concurrently
  void $ forConcurrently dmRelays $ \r -> do
    connected <- connect r -- no fallback for DM relays
    when connected $ subscribeToGiftwraps r xo

  -- Connect to inbox relays concurrently
  void $ forConcurrently inboxRelays $ \r -> do
    when (isInboxCapable r) $ do
      let relayUri = getUri r
      connected <- connect relayUri -- no fallback for inbox relays
      when connected $ subscribeToMentions relayUri xo

  followRelayMap <- buildRelayPubkeyMap (xo : followList) ownInboxRelayURIs

  connectedRelays <- forConcurrently (Map.toList followRelayMap) $ \(relayUri, pubkeys) -> do
    connected <- connect relayUri
    if connected
      then subscribeToProfilesAndPosts relayUri pubkeys
      else recordFailedRelay relayUri
    return (relayUri, connected)

  let failedRelays  = [ relayUri | (relayUri, connected) <- connectedRelays, not connected ]
      failedPubkeys = concatMap (\r -> Map.findWithDefault [] r followRelayMap) failedRelays

  unless (null failedPubkeys) $ do
    logInfo $ "Rebalancing subscriptions for pubkeys: " <> pack (show failedPubkeys)
    rebalanceSubscriptions failedPubkeys ownInboxRelayURIs

-- | Get relay lists for multiple pubkeys, making only one LMDB call per pubkey
getRelayListsForPubkeys :: InboxModelEff es => [PubKeyXO] -> Eff es (Map.Map PubKeyXO [Relay])
getRelayListsForPubkeys pks = do
    let uniquePks = Set.toList $ Set.fromList pks
    relayLists <- forM uniquePks $ \pk -> do
        relays <- getGeneralRelays pk
        return (pk, relays)
    return $ Map.fromList relayLists

-- | Try to connect to a relay, falling back to an alternative if it fails
connectWithFallback :: InboxModelEff es => RelayURI -> [PubKeyXO] -> Map.Map PubKeyXO [Relay] -> Eff es (Maybe RelayURI)
connectWithFallback relayUri pks relayMap = do
  connected <- connect relayUri
  if connected
    then return $ Just relayUri
    else do
      recordFailedRelay relayUri

      let outboxRelayURIs = [ getUri r
                            | pk <- pks
                            , r <- Map.findWithDefault [] pk relayMap
                            , isOutboxCapable r
                            , getUri r /= relayUri ]
      recentlyFailedRelays <- getFailedRelaysWithinLastNDays 5
      let workingRelays = filter (`notElem` recentlyFailedRelays) outboxRelayURIs

      case workingRelays of
        (alternativeRelay:_) -> do
          connected' <- connect alternativeRelay
          if connected'
            then return $ Just alternativeRelay
            else do
              recordFailedRelay alternativeRelay
              inboxRelays <- getGeneralRelays =<< (keyPairToPubKeyXO <$> getKeyPair)
              case [ getUri r | r <- inboxRelays, isInboxCapable r ] of
                (inboxUri:_) -> do
                  connected'' <- connect inboxUri
                  return $ if connected'' then Just inboxUri else Nothing
                [] -> return Nothing
        [] -> do
          inboxRelays <- getGeneralRelays =<< (keyPairToPubKeyXO <$> getKeyPair)
          case [ getUri r | r <- inboxRelays, isInboxCapable r ] of
            (inboxUri:_) -> do
              connected' <- connect inboxUri
              return $ if connected' then Just inboxUri else Nothing
            [] -> return Nothing


-- | Recursively rebalances subscriptions by re-computing the grouping for the given pubkeys using
-- the provided list of available inbox relay URIs (which are already filtered for inbox capability).
-- If any relay connection fails for its assigned pubkeys, that relay is removed from the available list,
-- and the function recurses with the pubkeys that still need a connection.
--
-- If the available fallback list becomes empty, rebalancing fails.
rebalanceSubscriptions 
  :: InboxModelEff es 
  => [PubKeyXO]  -- ^ Pubkeys that still need to connect.
  -> [RelayURI]  -- ^ Current available inbox relay URIs.
  -> Eff es ()
rebalanceSubscriptions pubkeys availableInboxURIs = do
  newRelayMap <- buildRelayPubkeyMap pubkeys availableInboxURIs
  newResults <- forConcurrently (Map.toList newRelayMap) $ \(relayUri, pkList) -> do
      connected <- connect relayUri
      if connected
        then subscribeToProfilesAndPosts relayUri pkList
        else recordFailedRelay relayUri
      return (relayUri, connected)
  let failedRelays    = [ relayUri | (relayUri, connected) <- newResults, not connected ]
      newFailedPubkeys = concatMap (\r -> Map.findWithDefault [] r newRelayMap) failedRelays
  if null newFailedPubkeys then
    return ()
  else do
    let newAvailable = filter (`notElem` failedRelays) availableInboxURIs
    if null newAvailable
      then logError $ "Rebalancing failed: no fallback relays available for pubkeys: " <> pack (show newFailedPubkeys)
      else rebalanceSubscriptions newFailedPubkeys newAvailable


-- | Subscribe to Giftwrap events on a relay
subscribeToGiftwraps :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToGiftwraps relayUri xo = do
  queue <- newTQueueIO
  lastTimestamp <- getSubscriptionTimestamp [xo] [GiftWrap]
  let lastTimestamp' = fmap (\ts -> ts - (2 * 24 * 60 * 60)) lastTimestamp  -- 2 days in seconds
  subId' <- subscribe relayUri (giftWrapFilter xo lastTimestamp') queue
  void $ async $ handleSubscription subId' queue

-- | Subscribe to mentions on a relay
subscribeToMentions :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToMentions relayUri xo = do
  queue <- newTQueueIO
  lastTimestamp <- getSubscriptionTimestamp [xo] [ShortTextNote, Repost, Comment, EventDeletion]
  subId' <- subscribe relayUri (mentionsFilter xo lastTimestamp) queue
  void $ async $ handleSubscription subId' queue


-- | Subscribe to profiles and posts for a relay
subscribeToProfilesAndPosts :: InboxModelEff es => RelayURI -> [PubKeyXO] -> Eff es ()
subscribeToProfilesAndPosts relayUri pks = do
    -- Subscribe to profiles
    queue <- newTQueueIO
    subId' <- subscribe relayUri (profilesFilter pks Nothing) queue
    void $ async $ handleSubscription subId' queue

    -- Subscribe to posts
    queue' <- newTQueueIO
    postsLastTimestamp <- getSubscriptionTimestamp pks [ShortTextNote, Repost, EventDeletion]
    subId'' <- subscribe relayUri (userPostsFilter pks postsLastTimestamp) queue'
    void $ async $ handleSubscription subId'' queue'


-- | Creates a filter to track changes in the relay network topology of contacts.
--   This filter monitors metadata and relationship events that can trigger relay
--   connection changes in the inbox model:
--
-- * RelayListMetadata (10002) - Contact's preferred general-purpose relays
-- * PreferredDMRelays (10003) - Contact's preferred DM relays
-- * FollowList (3) - Contact's social graph changes
-- * Metadata (0) - Contact's profile information
--
-- When these events are received, the inbox model may need to:
-- 1. Connect to new relays where contacts are publishing
-- 2. Disconnect from relays no longer used by contacts
-- 3. Update subscription states based on relationship changes
inboxRelayTopologyFilter :: [PubKeyXO] -> Filter
inboxRelayTopologyFilter pks = emptyFilter 
    { authors = Just pks
    , kinds = Just [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
    }


-- | Get the latest timestamp for a given pubkey and kind
getSubscriptionTimestamp :: InboxModelEff es => [PubKeyXO] -> [Kind] -> Eff es (Maybe Int)
getSubscriptionTimestamp pks ks = do
  timestamps <- forM pks $ \pk -> getLatestTimestamp pk ks
  if null timestamps 
    then return Nothing
    else case catMaybes timestamps of
      [] -> return Nothing
      ts -> return $ Just $ minimum ts


-- | Build a map from relay URI to pubkeys, using inbox relays only as fallback
buildRelayPubkeyMap :: InboxModelEff es => [PubKeyXO] -> [RelayURI] -> Eff es (Map.Map RelayURI [PubKeyXO])
buildRelayPubkeyMap pks ownInboxRelays = do
  recentlyFailedRelays <- getFailedRelaysWithinLastNDays 5
  relayPubkeyPairs <- forM pks $ \pk -> do
    relays <- getGeneralRelays pk
    let outboxRelayURIs = [ getUri r | r <- relays, isOutboxCapable r ]
        workingRelays = filter (`notElem` recentlyFailedRelays) outboxRelayURIs
        workingInboxRelays = filter (`notElem` recentlyFailedRelays) ownInboxRelays

        selectedRelays =
          if null workingRelays
            then workingInboxRelays
            else take maxRelaysPerContact workingRelays

    return (pk, selectedRelays)

  return $ Map.filter (not . null) $ foldr (\(pk, relays) acc ->
      foldr (\r acc' -> Map.insertWith (++) r [pk] acc') acc relays
    ) Map.empty relayPubkeyPairs


-- | Maximum number of outbox relays to consider per contact
maxRelaysPerContact :: Int
maxRelaysPerContact = 2


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

  relayMap <- getRelayListsForPubkeys (xo : followList)

  void $ forConcurrently inboxRelays $ \relay' -> do
    when (isInboxCapable relay') $ do
      let relayUri = getUri relay'
      mConnectedUri <- connectWithFallback relayUri [xo] relayMap
      forM_ mConnectedUri $ \uri ->
        subscribeToMentions uri xo

  newRelayPubkeyMap <- buildRelayPubkeyMap (xo : followList) ownInboxRelayURIs

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
    mConnectedUri <- connectWithFallback relayUri pubkeys relayMap
    forM_ mConnectedUri $ \uri ->
      subscribeToProfilesAndPosts uri pubkeys

  void $ forConcurrently (Set.toList relaysToUpdate) $ \relayUri -> do
    let newPubkeys = Set.fromList $ Map.findWithDefault [] relayUri newRelayPubkeyMap
    let currentPubkeys = Set.fromList $ getSubscribedPubkeys pool relayUri
    when (newPubkeys /= currentPubkeys) $ do
      stopAllSubscriptions relayUri
      mConnectedUri <- connectWithFallback relayUri (Set.toList newPubkeys) relayMap
      forM_ mConnectedUri $ \uri ->
        subscribeToProfilesAndPosts uri (Set.toList newPubkeys)


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

-- | Get pubkeys from subscriptions in RelayData
getSubscribedPubkeys :: RelayPool -> RelayURI -> [PubKeyXO]
getSubscribedPubkeys pool relayUri =
  [ pk
  | (_, sd) <- Map.toList (subscriptions pool)
  , relay sd == relayUri
  , pk <- fromMaybe [] (authors $ subscriptionFilter sd)
  ]
