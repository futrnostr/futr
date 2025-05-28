-- | Module: Nostr.InboxModel
-- Defines the inbox model for the Nostr protocol according to NIP-65.
-- https://github.com/nostr-protocol/nips/blob/master/65.md

{-# LANGUAGE BlockArguments #-}

module Nostr.InboxModel where

import Control.Monad (forever, forM, forM_, unless, void, when)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (pack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, cancel, forConcurrently)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets, put, modify)

import Logging
import Nostr
import Nostr.Event (EventId, Kind(..))
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Relay (Relay(..), RelayURI, defaultGeneralRelays, getUri, isInboxCapable, isOutboxCapable)
import Nostr.RelayConnection (RelayConnection, connect, disconnect, normalizeRelayURI)
import Nostr.Subscription
    ( Subscription
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
import Nostr.Types (Filter(..), SubscriptionId, emptyFilter)
import Nostr.Util
import QtQuick (QtQuick, UIUpdates(..), emptyUpdates, notify)
import RelayMgmt
import Store.Lmdb ( LmdbStore, getFollows, getGeneralRelays, getDMRelays
                  , getLatestTimestamp, getFailedRelaysWithinLastNDays, recordFailedRelay )
import Types ( AppState(..), ConnectionState(..), Follow(..), InboxModelState(..), SubscriptionState(..)
             , RelayPool(..), RelayData(..), initialRelayPool )


-- | InboxModel effects
data InboxModel :: Effect where
  StartInboxModel :: InboxModel m ()
  StopInboxModel :: InboxModel m ()
  AwaitAtLeastOneConnected :: InboxModel m Bool
  SubscribeToProfilesAndPostsFor :: PubKeyXO -> InboxModel m [SubscriptionId]
  SubscribeToCommentsFor :: EventId -> InboxModel m ()
  UnsubscribeToCommentsFor :: EventId -> InboxModel m ()

type instance DispatchOf InboxModel = Dynamic


startInboxModel :: InboxModel :> es => Eff es ()
startInboxModel = send StartInboxModel

stopInboxModel :: InboxModel :> es => Eff es ()
stopInboxModel = send StopInboxModel

awaitAtLeastOneConnected :: InboxModel :> es => Eff es Bool
awaitAtLeastOneConnected = send AwaitAtLeastOneConnected

subscribeToProfilesAndPostsFor :: InboxModel :> es => PubKeyXO -> Eff es [SubscriptionId]
subscribeToProfilesAndPostsFor pk = send $ SubscribeToProfilesAndPostsFor pk

subscribeToCommentsFor :: InboxModel :> es => EventId -> Eff es ()
subscribeToCommentsFor eid = send $ SubscribeToCommentsFor eid

unsubscribeToCommentsFor :: InboxModel :> es => EventId -> Eff es ()
unsubscribeToCommentsFor eid = send $ UnsubscribeToCommentsFor eid


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

    -- Create a background processing queue for metadata and subscription updates
    -- This queue system handles changes to:
    --   1. Follow lists (adding/removing contacts)
    --   2. Relay preferences (general or DM relays)
    --   3. Other metadata that affects our subscription topology
    --
    -- Updates are batched together to prevent overwhelming the system with
    -- rapid consecutive changes. During bootstrap mode, updates are dequeued
    -- but not processed to prevent race conditions while initial connections
    -- are being established. Once bootstrap completes, normal processing resumes.
    --
    -- The queue is designed to be thread-safe, allowing updates to be triggered
    -- from any part of the application without blocking.
    updateQueue' <- newTQueueIO
    inBootstrapVar <- atomically $ newTVar True  -- Local variable, not stored in RelayPool
    updateThread' <- async $ forever $ do
      void $ atomically $ readTQueue updateQueue'

      inBootstrap <- atomically $ readTVar inBootstrapVar
      unless inBootstrap $ do
        void $ atomically $ flushTQueue updateQueue'
        updateSubscriptions xo

      threadDelay 10000000  -- Relay updates are processed every 10 seconds

    modify @RelayPool $ \s -> s
        { updateQueue = updateQueue'
        , updateThread = Just updateThread'
        }

    -- initialize the inbox model
    inboxRelays <- getGeneralRelays xo
    changeInboxModelState InitialBootstrap
    if null inboxRelays
      then initializeWithDefaultRelays xo
      else continueWithRelays inboxRelays

    -- Once initial setup is complete, exit bootstrap mode
    atomically $ writeTVar inBootstrapVar False
    changeInboxModelState LiveProcessing

  StopInboxModel -> do
    st <- get @RelayPool

    forM_ (updateThread st) cancel
    forM_ (monitoringThread st) cancel

    modify @RelayPool $ \s -> s { updateThread = Nothing }

    forM_ (Map.keys $ activeConnections st) $ \relayUri -> do
      disconnect relayUri

    put @RelayPool initialRelayPool
    changeInboxModelState Stopped

  AwaitAtLeastOneConnected -> awaitAtLeastOneConnected'

  SubscribeToProfilesAndPostsFor xo -> do
      kp <- getKeyPair
      let myXO = keyPairToPubKeyXO kp
      conns <- gets @RelayPool activeConnections

      let connectedRelays = Map.keys $ Map.filter (\rd -> connectionState rd == Connected) conns
      let bootstrapFilter = inboxRelayTopologyFilter [xo]

      -- Handle bootstrap subscriptions - we don't collect these IDs since they're already terminated
      forM_ connectedRelays $ \r -> do
          subId' <- subscribe r bootstrapFilter
          void $ handleSubscriptionUntilEOSE subId'

      ownInboxRelayURIs <- map getUri <$> getGeneralRelays myXO
      followRelayMap <- buildRelayPubkeyMap [xo] ownInboxRelayURIs
      pubkeyTimestamps <- getPubkeyTimestamps [xo]

      let currentConnections = Map.keysSet $ Map.filter (\rd -> connectionState rd == Connected) conns

      forM_ (Map.toList followRelayMap) $ \(relayUri, _) -> do
          let alreadyConnected = relayUri `Set.member` currentConnections
          unless alreadyConnected $ do
              void $ connect relayUri

      allSubIds <- fmap concat $ forM (Map.toList followRelayMap) $ \(relayUri, pubkeys) -> do
          st <- get @RelayPool
          let isConnected = case Map.lookup relayUri (activeConnections st) of
                              Just rd -> connectionState rd == Connected
                              Nothing -> False
          if isConnected
              then subscribeToProfilesAndPosts relayUri pubkeys pubkeyTimestamps
              else return []

      return allSubIds

  SubscribeToCommentsFor eid -> do
    pool <- get @RelayPool
    let activeRelays = Map.keys $ activeConnections pool

    forM_ activeRelays $ \relayUri -> do
      subId' <- subscribe relayUri (commentsFilter eid)
      modify @RelayPool $ \s -> s { commentSubscriptions = Map.insertWith (++) eid [subId'] (commentSubscriptions s) }
      void $ async $ handlePaginationSubscription subId'

  UnsubscribeToCommentsFor eid -> do
    pool <- get @RelayPool
    let subIds = Map.findWithDefault [] eid (commentSubscriptions pool)

    forM_ subIds stopSubscription

    modify @RelayPool $ \s ->
      s { commentSubscriptions = Map.delete eid (commentSubscriptions pool) }


changeInboxModelState :: InboxModelEff es => InboxModelState -> Eff es ()
changeInboxModelState state = do
  modify @AppState $ \s -> s { inboxModelState = state }
  notify $ emptyUpdates { inboxModelStateChanged = True }


-- | Wait up to 3 seconds for at least one relay to connect
awaitAtLeastOneConnected' :: InboxModelEff es => Eff es Bool
awaitAtLeastOneConnected' = do
    let maxAttempts = 300  -- 15 seconds total (300 * 50ms)
        loop :: InboxModelEff es => Int -> Eff es Bool
        loop 0 = return False  -- No more attempts left
        loop n = do
            st <- get @RelayPool
            let states = map (connectionState . snd) $ Map.toList $ activeConnections st
            if any (== Connected) states
                then return True
                else do
                    threadDelay 50000  -- 50ms delay
                    loop (n - 1)
    loop maxAttempts

-- | Initialize using default relays when no relay configuration exists
initializeWithDefaultRelays :: InboxModelEff es => PubKeyXO -> Eff es ()
initializeWithDefaultRelays xo = do
  let (defaultRelays, _) = defaultGeneralRelays

  connectionResults <- forConcurrently defaultRelays $ \r -> do
    connected <- connect (getUri r)
    return (r, connected)

  let connectedRelays = [r | (r, success) <- connectionResults, success]
  let bootstrapFilter = inboxRelayTopologyFilter [xo]

  void $ forConcurrently connectedRelays $ \r -> do
      subId' <- subscribe (getUri r) bootstrapFilter
      void $ handleSubscriptionUntilEOSE subId'
      follows <- getFollows xo
      unless (null follows) $ do
        let followFilter = inboxRelayTopologyFilter $ map pubkey follows
        subId'' <- subscribe (getUri r) followFilter
        void $ handleSubscriptionUntilEOSE subId''

  myGeneralRelays <- getGeneralRelays xo
  myDMRelays <- getDMRelays xo

  when (null myGeneralRelays) setDefaultGeneralRelays
  when (null myDMRelays) setDefaultDMRelays

  myGeneralRelays' <- getGeneralRelays xo
  continueWithRelays myGeneralRelays'


-- | Calculate timestamp maps for a list of pubkeys
-- Returns a map from pubkey to (profileTimestamp, postsTimestamp)
getPubkeyTimestamps :: InboxModelEff es => [PubKeyXO] -> Eff es (Map.Map PubKeyXO (Maybe Int, Maybe Int))
getPubkeyTimestamps pubkeys = Map.fromList <$> forM pubkeys \pk -> do
    profileTs <- getLatestTimestamp pk [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
    postsTs <- getLatestTimestamp pk [ShortTextNote, Repost, EventDeletion]
    return (pk, (profileTs, postsTs))


-- | Continue with discovered relays
continueWithRelays :: InboxModelEff es => [Relay] -> Eff es ()
continueWithRelays inboxRelays = do
  changeInboxModelState SyncingHistoricData
  kp <- getKeyPair
  let xo = keyPairToPubKeyXO kp
  let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]

  dmRelays <- getDMRelays xo
  follows <- getFollows xo
  let followList = xo : map pubkey follows

  -- Connect to DM relays concurrently with rate limiting
  void $ forConcurrently dmRelays $ \r -> do
    connected <- connect r -- no fallback for DM relays
    when connected $ subscribeToGiftwraps r xo

  -- Connect to inbox relays concurrently with rate limiting
  void $ forConcurrently inboxRelays $ \r -> do
    when (isInboxCapable r) $ do
      let relayUri = getUri r
      connected <- connect relayUri -- no fallback for inbox relays
      when connected $ subscribeToMentions relayUri xo

  followRelayMap <- buildRelayPubkeyMap (xo : followList) ownInboxRelayURIs

  -- Pre-calculate timestamps for each pubkey
  pubkeyTimestamps <- getPubkeyTimestamps (xo : followList)

  -- Connect to follow relays with rate limiting (max 7 concurrent new connections)
  let relayChunks = chunksOf 7 (Map.toList followRelayMap)
  connectedRelays <- fmap concat $ forM relayChunks $ \chunk -> do
    results <- forConcurrently chunk $ \(relayUri, pubkeys) -> do
      connected <- connect relayUri
      if connected
        then void $ subscribeToProfilesAndPosts relayUri pubkeys pubkeyTimestamps
        else recordFailedRelay relayUri
      return (relayUri, connected)
    -- Add delay between chunks to avoid overwhelming the network
    threadDelay 250000  -- 250ms delay between chunks
    return results

  let failedRelays  = [ relayUri | (relayUri, connected) <- connectedRelays, not connected ]
      failedPubkeys = concatMap (\r -> Map.findWithDefault [] r followRelayMap) failedRelays

  unless (null failedPubkeys) $ do
    logInfo $ "Rebalancing subscriptions for pubkeys: " <> pack (show failedPubkeys)
    rebalanceSubscriptions failedPubkeys ownInboxRelayURIs


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

  -- Calculate timestamps for each pubkey
  pubkeyTimestamps <- getPubkeyTimestamps pubkeys

  newResults <- forConcurrently (Map.toList newRelayMap) $ \(relayUri, pkList) -> do
      connected <- connect relayUri
      if connected
        then void $ subscribeToProfilesAndPosts relayUri pkList pubkeyTimestamps
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
  lastTimestamp <- getSubscriptionTimestamp [xo] [GiftWrap]
  let lastTimestamp' = fmap (\ts -> ts - (2 * 24 * 60 * 60)) lastTimestamp  -- 2 days in seconds
      f = (giftWrapFilter xo lastTimestamp') { limit = Just 2000 }
  subId' <- subscribe relayUri f
  void $ async $ handlePaginationSubscription subId'


-- | Subscribe to mentions on a relay
subscribeToMentions :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToMentions relayUri xo = do
  lastTimestamp <- getSubscriptionTimestamp [xo] [ShortTextNote, Repost, Comment, EventDeletion]
  subId' <- subscribe relayUri (mentionsFilter xo lastTimestamp)
  void $ async $ handlePaginationSubscription subId'


-- | Subscribe to profiles and posts with pre-calculated timestamps for each pubkey
subscribeToProfilesAndPosts :: InboxModelEff es
                            => RelayURI
                            -> [PubKeyXO]
                            -> Map.Map PubKeyXO (Maybe Int, Maybe Int)  -- Map of (profileTs, postsTs)
                            -> Eff es [SubscriptionId]
subscribeToProfilesAndPosts relayUri pks timestampMap = do
    -- Subscribe to profiles - Metadata generally doesn't need timestamp filtering
    let profileFilter = profilesFilter pks
    --logDebug $ "Subscribing to profiles for " <> pack (show pks) <> " on relay " <> relayUri
    --logDebug $ "Profile filter: " <> pack (show profileFilter)
    subId' <- subscribe relayUri profileFilter
    void $ async $ handlePaginationSubscription subId'

    -- Subscribe to posts with precise timestamps for each pubkey
    -- Get the effective timestamps for this specific set of pubkeys
    let postsTimestamps = [ts | pk <- pks, Just ts <- [snd =<< Map.lookup pk timestampMap]]
        effectivePostsTimestamp = if null postsTimestamps then Nothing else Just (maximum postsTimestamps)

    let postsFilter = userPostsFilter pks effectivePostsTimestamp Nothing
    subId'' <- subscribe relayUri postsFilter
    void $ async $ handlePaginationSubscription subId''

    -- Return both subscription IDs
    return [subId', subId'']


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
  timestamps <- forM pks $ \pk -> do
    r <- getLatestTimestamp pk ks
    return r
  if null timestamps
    then return Nothing
    else case catMaybes timestamps of
      [] -> return Nothing
      ts -> return $ Just $ maximum ts


-- | Build a map from relay URI to pubkeys, using inbox relays only as fallback
buildRelayPubkeyMap :: InboxModelEff es => [PubKeyXO] -> [RelayURI] -> Eff es (Map.Map RelayURI [PubKeyXO])
buildRelayPubkeyMap pks ownInboxRelays = do
  recentlyFailedRelays <- getFailedRelaysWithinLastNDays 1
  relayPubkeyPairs <- forM pks $ \pk -> do
    relays <- getGeneralRelays pk
    let outboxRelayURIs = [ normalizeRelayURI (getUri r) | r <- relays, isOutboxCapable r ]
        workingRelays = filter (`notElem` recentlyFailedRelays) outboxRelayURIs
        workingInboxRelays = filter (`notElem` recentlyFailedRelays)
                           $ map normalizeRelayURI ownInboxRelays
        selectedRelays =
          if null workingRelays
            then workingInboxRelays
            else take maxRelaysPerContact workingRelays

    return (pk, selectedRelays)

  let relayToPubkeysMap = foldr
        (\(pk, relays) acc ->
          foldr
            (\r acc' -> Map.insertWith Set.union r (Set.singleton pk) acc')
            acc
            relays
        )
        Map.empty
        relayPubkeyPairs

  return $ Map.map Set.toList relayToPubkeysMap


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
  let followList = xo : map pubkey follows

  inboxRelays <- getGeneralRelays xo
  let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]

  pubkeyTimestamps <- getPubkeyTimestamps followList

  -- Handle mentions subscriptions for inbox-capable relays
  void $ forConcurrently inboxRelays $ \relay' -> do
    when (isInboxCapable relay') $ do
      let relayUri = getUri relay'
      st <- get @RelayPool
      let isAlreadyConnected = Map.member relayUri (activeConnections st)
      connected <- if isAlreadyConnected
                    then pure True
                    else connect relayUri  -- No fallback for inbox relays
      when connected $ subscribeToMentions relayUri xo

  -- Build new relay-pubkey mapping for follows
  newRelayPubkeyMap <- buildRelayPubkeyMap followList ownInboxRelayURIs

  st <- get @RelayPool
  let currentRelays = Map.keysSet (activeConnections st)
  let newRelays = Map.keysSet newRelayPubkeyMap

  let relaysToAdd = Set.difference newRelays currentRelays
  let relaysToRemove = Set.difference currentRelays newRelays
  let relaysToUpdate = Set.intersection currentRelays newRelays
  -- Remove old connections
  void $ forConcurrently (Set.toList relaysToRemove) $ \relayUri -> do
    disconnect relayUri

  -- Add new connections
  void $ forConcurrently (Set.toList relaysToAdd) $ \relayUri -> do
    let pubkeys = Map.findWithDefault [] relayUri newRelayPubkeyMap
    stopAllSubscriptions relayUri
    st' <- get @RelayPool
    let isAlreadyConnected = Map.member relayUri (activeConnections st')
    connected <- if isAlreadyConnected
                  then pure True
                  else connect relayUri
    if connected
      then void $ subscribeToProfilesAndPosts relayUri pubkeys pubkeyTimestamps
      else do
        recordFailedRelay relayUri
        rebalanceSubscriptions pubkeys ownInboxRelayURIs

  -- Update existing connections
  void $ forConcurrently (Set.toList relaysToUpdate) $ \relayUri -> do
    let newPubkeys = Map.findWithDefault [] relayUri newRelayPubkeyMap
    stopAllSubscriptions relayUri
    st' <- get @RelayPool
    let isAlreadyConnected = Map.member relayUri (activeConnections st')
    connected <- if isAlreadyConnected
                  then pure True
                  else connect relayUri
    if connected
      then void $ subscribeToProfilesAndPosts relayUri newPubkeys pubkeyTimestamps
      else do
        recordFailedRelay relayUri
        rebalanceSubscriptions newPubkeys ownInboxRelayURIs


-- | Update DM subscriptions
updateDMSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateDMSubscriptions xo = do
    kp <- getKeyPair
    let ownPubkey = keyPairToPubKeyXO kp

    when (xo == ownPubkey) $ do
        dmRelayURIs <- getDMRelays xo

        let dmRelaySet = Set.fromList dmRelayURIs
        st <- get @RelayPool

        let giftwrapSubs =
              [ (relayUri, subId)
              | (subId, sd) <- Map.toList (subscriptions st)
              , GiftWrap `elem` fromMaybe [] (kinds $ subscriptionFilter sd)
              , relayUri <- Map.keys (activeConnections st)
              , not (relayUri `Set.member` dmRelaySet)
              ]

        -- Stop giftwrap subscriptions for relays not in dmRelaySet
        void $ forConcurrently giftwrapSubs $ \(relayUri, subId) -> do
            stopSubscription subId
            let hasOtherSubs = any (\(sid, sd) -> sid /= subId && relay sd == relayUri)
                                  (Map.toList $ subscriptions st)

            when (not hasOtherSubs) $ disconnect relayUri

        let currentRelaySet = Map.keysSet (activeConnections st)
        let relaysToAdd = Set.difference dmRelaySet currentRelaySet

        void $ forConcurrently (Set.toList relaysToAdd) $ \relayUri -> do
            st' <- get @RelayPool
            let isAlreadyConnected = Map.member relayUri (activeConnections st')

            connected <- if isAlreadyConnected
                          then return True
                          else connect relayUri
            if connected
                then do
                    stopAllSubscriptions relayUri
                    subscribeToGiftwraps relayUri xo
                else recordFailedRelay relayUri
