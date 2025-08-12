-- | Module: Nostr.InboxModel
-- Defines the inbox model for the Nostr protocol according to NIP-65.
-- https://github.com/nostr-protocol/nips/blob/master/65.md

{-# LANGUAGE BlockArguments #-}

module Nostr.InboxModel where

import Control.Monad (forever, forM, forM_, unless, void, when)
import Data.List.Split (chunksOf)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, cancel, forConcurrently)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets, put, modify)
import qualified Nostr.RelayPool as RelayPool
import Prelude hiding (until)

import Logging
import Nostr
import Nostr.Event (EventId, Kind(..), getEventId)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO, exportPubKeyXO, byteStringToHex)
import Nostr.Relay (Relay(..), RelayURI, defaultDMRelays, defaultGeneralRelays, getUri, isInboxCapable, isOutboxCapable)
import Nostr.RelayConnection (RelayConnection, connect, disconnect)
import Nostr.Relay (normalizeRelayURI)
import Nostr.Subscription ( Subscription
                          , stopAllSubscriptions, stopSubscription
                          , subscribe, subscribeTemporary
                          , subscribeMultiTemporary, awaitAllEose
                          , giftWrapFilter)
import Nostr.FilterSet (inboxMentionsFilter, profilesFilterFS, userPostsFilterFS, commentsFilterFS, filtersStructurallyChanged)
import Nostr.FeedKeys (feedKeyForPosts, feedKeyForMentions, feedKeyForComments)
import qualified Data.ByteString.Base16 as B16
import Data.Text.Encoding (decodeUtf8)
import Nostr.SubscriptionHandler
import Nostr.Types (Filter(..), SubscriptionId, emptyFilter)
import Nostr.Types qualified as NT
import Nostr.Util
import QtQuick (QtQuick, UIUpdates(..), emptyUpdates, notify)
import RelayMgmt
import Store.Lmdb ( LmdbStore, getFollows, getGeneralRelays, getDMRelays
                  , getOrSeedAnchor )
import Types ( AppState(..), ConnectionState(..), Follow(..), InboxModelState(..) )
import Nostr.RelayPool (RelayPool, RelayPoolState(..), RelayData(..), SubscriptionState(..), initialRelayPool)


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
  , State RelayPoolState :> es
  , RelayPool :> es
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
      inBootstrap <- atomically $ readTVar inBootstrapVar
      if inBootstrap
        then do
          threadDelay 1000000  -- Check every 1 second during bootstrap
        else do
          void $ atomically $ readTQueue updateQueue'
          void $ atomically $ flushTQueue updateQueue'
          updateSubscriptions xo
          threadDelay 5000000  -- Relay updates are processed every 5 seconds

    RelayPool.setUpdateThread (Just updateThread')

    -- Periodic stats logger
    statsThread' <- async $ forever $ do
      st <- get @RelayPoolState
      let conns = Map.size (activeConnections st)
          subs  = Map.size (subscriptions st)
          pend  = Map.size (pendingSubscriptions st)
      logDebug $ "STATS: conns=" <> T.pack (show conns)
              <> ", subs=" <> T.pack (show subs)
              <> ", pendingSubs=" <> T.pack (show pend)
      -- per-relay brief stats with deltas
      let prev = lastStatsSnapshot st
      let (newSnapshot, linesOut) = foldl (\(accSnap, accLines) (uri, rd) ->
              let prevVals = Map.lookup uri prev
                  msgs = messagesReceived rd
                  evts = eventsReceived rd
                  (dMsgs, dEvts) = case prevVals of
                                    Just (pm, pe) -> (msgs - pm, evts - pe)
                                    Nothing -> (msgs, evts)
                  prSz = length (pendingRequests rd)
                  peSz = length (pendingEvents rd)
                  line = "RELAY: " <> uri <>
                         " msgs=" <> T.pack (show msgs) <>
                         "(d=" <> T.pack (show dMsgs) <> ") evts=" <> T.pack (show evts) <>
                         "(d=" <> T.pack (show dEvts) <> ") notices=" <> T.pack (show (noticeCount rd)) <>
                         " pr=" <> T.pack (show prSz) <>
                         " pe=" <> T.pack (show peSz)
              in (Map.insert uri (msgs, evts) accSnap, line:accLines)
            ) (Map.empty, []) (Map.toList $ activeConnections st)
      mapM_ logDebug (reverse linesOut)
      RelayPool.setLastStatsSnapshot newSnapshot
      -- coverage audit: are all follows assigned to at least one relay?
      kp <- getKeyPair
      let myXO = keyPairToPubKeyXO kp
      follows <- getFollows myXO
      inboxRelays' <- getGeneralRelays myXO
      let ownInboxRelayURIs = [ getUri r | r <- inboxRelays', isInboxCapable r ]
          followPks = map pubkey follows
      relayMap <- pickRelaysForAuthors followPks ownInboxRelayURIs
      let covered = Set.fromList (concat (Map.elems relayMap))
          totalFollows = length followPks
          coveredCount = Set.size covered
          uncoveredCount = totalFollows - coveredCount
          perRelayCounts = [ (uri, length pks) | (uri, pks) <- Map.toList relayMap ]
      logDebug $ "COVERAGE: follows=" <> T.pack (show totalFollows)
              <> ", covered=" <> T.pack (show coveredCount)
              <> ", uncovered=" <> T.pack (show uncoveredCount)
      logDebug $ "COVERAGE_PER_RELAY: " <> T.pack (show perRelayCounts)
      threadDelay (10 * 1000000)
    RelayPool.setStatsThread (Just statsThread')

    -- Periodic reconciliation: refresh relay mapping and resubscribe when needed
    reconcileThread' <- async $ forever $ do
      kp <- getKeyPair
      let myXO = keyPairToPubKeyXO kp
      updateGeneralSubscriptions myXO
      -- Re-evaluate existing subscriptions' lastSentFilter vs current filter and resend if structural change
      stRes <- get @RelayPoolState
      forM_ (Map.toList (subscriptions stRes)) $ \(sid, sd) -> do
        let oldF = lastSentFilter sd
            newF = subscriptionFilter sd
        case (oldF, Map.lookup (relay sd) (activeConnections stRes)) of
          (Just ofilt, Just rd) | filtersStructurallyChanged ofilt newF -> do
            atomically $ writeTChan (requestChannel rd) (NT.Subscribe $ NT.Subscription sid newF)
            RelayPool.setSubscriptionLastSentFilter sid newF
          _ -> pure ()
      threadDelay (30 * 1000000) -- every 30s
    RelayPool.setReconcileThread (Just reconcileThread')

    inboxRelays <- getGeneralRelays xo
    changeInboxModelState InitialBootstrap

    when (null inboxRelays) $ initializeWithDefaultRelays xo

    atomically $ writeTVar inBootstrapVar False
    atomically $ writeTQueue updateQueue' ()

    inboxRelays' <- getGeneralRelays xo
    unless (null inboxRelays') $ do
      continueWithRelays inboxRelays'

  StopInboxModel -> do
    st <- get @RelayPoolState

    forM_ (updateThread st) cancel
    forM_ (statsThread st) cancel
    forM_ (reconcileThread st) cancel

    RelayPool.setUpdateThread Nothing

    forM_ (Map.keys $ activeConnections st) $ \relayUri -> do
      disconnect relayUri

    put @RelayPoolState initialRelayPool
    changeInboxModelState Stopped

  AwaitAtLeastOneConnected -> do
    let maxAttempts = 300  -- 15 seconds total (300 * 50ms)
        loop :: InboxModelEff es => Int -> Eff es Bool
        loop 0 = return False  -- No more attempts left
        loop n = do
            st <- get @RelayPoolState
            let states = map (connectionState . snd) $ Map.toList $ activeConnections st
            if any (== Connected) states
                then return True
                else do
                    threadDelay 50000  -- 50ms delay
                    loop (n - 1)
    loop maxAttempts

  SubscribeToProfilesAndPostsFor xo -> do
      kp <- getKeyPair
      let myXO = keyPairToPubKeyXO kp
      conns <- gets @RelayPoolState activeConnections

      let connectedRelays = Map.keys $ Map.filter (\rd -> connectionState rd == Connected) conns
      let bootstrapFilter = inboxRelayTopologyFilter [xo]

      forM_ connectedRelays $ \r -> void $ subscribe r bootstrapFilter

      ownInboxRelayURIs <- map getUri <$> getGeneralRelays myXO
      followRelayMap <- pickRelaysForAuthors [xo] ownInboxRelayURIs

      let currentConnections = Map.keysSet $ Map.filter (\rd -> connectionState rd == Connected) conns

      forM_ (Map.toList followRelayMap) $ \(relayUri, _) -> do
          let alreadyConnected = relayUri `Set.member` currentConnections
          unless alreadyConnected $ do
              void $ connect relayUri

      allSubIds <- fmap concat $ forM (Map.toList followRelayMap) $ \(relayUri, pubkeys) -> do
          st <- get @RelayPoolState
          let isConnected = case Map.lookup relayUri (activeConnections st) of
                              Just rd -> connectionState rd == Connected
                              Nothing -> False
          if isConnected
              then do sid <- subscribeToProfilesAndPosts relayUri pubkeys
                      return [sid]
              else return []

      return allSubIds

  SubscribeToCommentsFor eid -> do
    pool <- get @RelayPoolState
    let activeRelays = Map.keys $ activeConnections pool
    forM_ activeRelays $ \relayUri -> do
      let key = feedKeyForComments (decodeUtf8 $ B16.encode $ getEventId eid)
      a0 <- getOrSeedAnchor relayUri key
      let baseFilter = commentsFilterFS eid
          anchoredFilter = baseFilter { since = Just a0 }
      subId' <- subscribe relayUri anchoredFilter
      RelayPool.insertCommentSubscription eid subId'

  UnsubscribeToCommentsFor eid -> do
    pool <- get @RelayPoolState
    let subIds = Map.findWithDefault [] eid (commentSubscriptions pool)
    forM_ subIds stopSubscription
    RelayPool.deleteCommentSubscription eid


changeInboxModelState :: InboxModelEff es => InboxModelState -> Eff es ()
changeInboxModelState state = do
  modify @AppState $ \s -> s { inboxModelState = state }
  notify $ emptyUpdates { inboxModelStateChanged = True }


-- | Initialize using default relays when no relay configuration exists
initializeWithDefaultRelays :: InboxModelEff es => PubKeyXO -> Eff es ()
initializeWithDefaultRelays xo = do
  let (defaultRelays, _) = defaultGeneralRelays

  connectionResults <- forConcurrently defaultRelays $ \r -> do
    connected <- connect $ getUri r
    return (r, connected)

  let connectedRelays = [r | (r, success) <- connectionResults, success]
      bootstrapFilter = inboxRelayTopologyFilter [xo]

  void $ forConcurrently connectedRelays $ \r -> do
      void $ subscribe (getUri r) bootstrapFilter
      follows <- getFollows xo
      unless (null follows) $ do
        let followFilter = inboxRelayTopologyFilter $ map pubkey follows
        void $ subscribe (getUri r) followFilter

  myGeneralRelays <- getGeneralRelays xo
  myDMRelays <- getDMRelays xo

  when (null myGeneralRelays) setDefaultGeneralRelays
  when (null myDMRelays) setDefaultDMRelays


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
      isAlreadyConnected <- RelayPool.isActiveConnection relayUri
      connected <- if isAlreadyConnected then pure True else connect relayUri -- no fallback for inbox relays
      when connected $ do
        -- Only add mentions subs once per relay; avoid duplicates across update cycles
        st1 <- get @RelayPoolState
        let hasMentions = any (\(_, sd) -> relay sd == relayUri && isMentionsFilter (subscriptionFilter sd)) (Map.toList $ subscriptions st1)
        unless hasMentions $ subscribeToMentions relayUri xo

  -- Compute assignments using the picker pipeline
  pickedMap <- pickRelaysForAuthors followList ownInboxRelayURIs

  -- Chunk authors per relay to avoid multi-author caps
  let chunkAuthors n as = chunksOf n as
      chunkedRelayMap = Map.fromList
        [ (relayUri, chunkAuthors 20 pks) | (relayUri, pks) <- Map.toList pickedMap ]

  -- Connect to follow relays with rate limiting (max 7 concurrent new connections)
  let relayChunks = chunksOf 7 (Map.toList chunkedRelayMap)
  connectedRelays <- fmap concat $ forM relayChunks $ \chunk -> do
    results <- forConcurrently chunk $ \(relayUri, pkChunks) -> do
      connected <- connect relayUri
      if connected
        then do
          -- subscribe per chunk
          void $ forM pkChunks $ \pks ->
            void $ subscribeToProfilesAndPosts relayUri pks
        else pure ()
      return (relayUri, connected)
    -- Add delay between chunks to avoid overwhelming the network
    threadDelay 250000  -- 250ms delay between chunks
    return results

  let failedRelays  = [ relayUri | (relayUri, connected) <- connectedRelays, not connected ]
      failedPubkeys = concatMap (\r -> Map.findWithDefault [] r pickedMap) failedRelays

  unless (null failedPubkeys) $ do
    rebalanceSubscriptions failedPubkeys ownInboxRelayURIs

  changeInboxModelState LiveProcessing


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
rebalanceSubscriptions _ [] = pure ()
rebalanceSubscriptions pubkeys availableInboxURIs = do
  newRelayMap <- pickRelaysForAuthors pubkeys availableInboxURIs

  newResults <- forConcurrently (Map.toList newRelayMap) $ \(relayUri, pkList) -> do
      connected <- connect relayUri
      if connected
        then void $ subscribeToProfilesAndPosts relayUri pkList
        else pure ()
      return (relayUri, connected)
  let failedRelays    = [ relayUri | (relayUri, connected) <- newResults, not connected ]
      newFailedPubkeys = concatMap (\r -> Map.findWithDefault [] r newRelayMap) failedRelays
      newAvailable = filter (`notElem` failedRelays) availableInboxURIs
  rebalanceSubscriptions newFailedPubkeys newAvailable


-- | Subscribe to Giftwrap events on a relay
subscribeToGiftwraps :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToGiftwraps relayUri xo = do
  -- Seed anchor for giftwraps per relay and author, then backdate 2 days for safety
  let key = feedKeyForMentions xo -- optional: create dedicated feedKeyForGiftwraps if desired
  a0 <- getOrSeedAnchor relayUri key
  let sinceTs = max 0 (a0 - 2 * 24 * 60 * 60)
      f = (giftWrapFilter xo (Just sinceTs)) { limit = Just 2000 }
  void $ subscribe relayUri f


-- | Subscribe to mentions on a relay
subscribeToMentions :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToMentions relayUri xo = do
  now <- getCurrentTime
  a0 <- getOrSeedAnchor relayUri (feedKeyForMentions xo)
  st <- get @RelayPoolState
  follows <- getFollows xo
  let followerAuthors = xo : map pubkey follows
  -- If spam-safety is enabled, restrict authors to follows for this subscription
  let maybeAuthors = if avoidSpamOnUnsafeRelays st then Just followerAuthors else Nothing
  -- Live mentions (since now)
  let liveBase = (inboxMentionsFilter xo (Just now)) { limit = Nothing }
      liveFilter = liveBase { authors = chooseAuthors maybeAuthors (authors liveBase) }
  void $ subscribe relayUri liveFilter
  -- Historic mentions up to now (finite, temporary)
  let histBase = (inboxMentionsFilter xo (Just a0)) { until = Just now, limit = Just 2000 }
      histFilter = histBase { authors = chooseAuthors maybeAuthors (authors histBase) }
  void $ subscribeTemporary relayUri histFilter

  where
    chooseAuthors :: Maybe [PubKeyXO] -> Maybe [PubKeyXO] -> Maybe [PubKeyXO]
    chooseAuthors (Just xs) _ = Just xs
    chooseAuthors Nothing b  = b

-- Identify if a filter is a mentions filter (by presence of 'p' tag and kinds)
isMentionsFilter :: Filter -> Bool
isMentionsFilter f =
  case (kinds f, fTags f) of
    (Just ks, Just tags) ->
      let hasKinds = all (`elem` ks) [ShortTextNote, Repost, Comment, EventDeletion]
          hasPTags = Map.member 'p' tags
      in hasKinds && hasPTags
    _ -> False


-- | Subscribe to profiles and posts with pre-calculated timestamps for each pubkey
subscribeToProfilesAndPosts :: InboxModelEff es
                            => RelayURI
                            -> [PubKeyXO]
                            -> Eff es SubscriptionId
subscribeToProfilesAndPosts relayUri pks = do
    -- Subscribe to profiles - Metadata generally doesn't need timestamp filtering
    st <- get @RelayPoolState
    kp <- getKeyPair
    let xo = keyPairToPubKeyXO kp
    follows <- getFollows xo
    let followerAuthors = xo : map pubkey follows
    let profileBase = profilesFilterFS pks
        profileFilter = if avoidSpamOnUnsafeRelays st then profileBase { authors = Just followerAuthors } else profileBase
    --logDebug $ "Subscribing to profiles for " <> pack (show pks) <> " on relay " <> relayUri
    --logDebug $ "Profile filter: " <> pack (show profileFilter)
    void $ subscribe relayUri profileFilter

    -- Concurrent live + historic posts subscriptions
    now <- getCurrentTime

    -- Live: after 'now' to capture new events that arrive while we paginate history
    let liveBase = (userPostsFilterFS pks (Just now) Nothing) { limit = Nothing }
        liveFilter = if avoidSpamOnUnsafeRelays st then liveBase { authors = Just followerAuthors } else liveBase
    liveSubId <- subscribe relayUri liveFilter
    -- No pagination for live

    -- Historic: anchors-only. Use per-relay feed anchor for this author group (seeded if absent)
    anchor <- getOrSeedAnchor relayUri (feedKeyForPosts pks)
    let base0 = userPostsFilterFS pks (Just anchor) Nothing
        historicBase = if avoidSpamOnUnsafeRelays st then base0 { authors = Just followerAuthors } else base0
        historicFilter = historicBase { until = Just now }
    void $ subscribeTemporary relayUri historicFilter

    return liveSubId


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


-- | End-to-end picker: derive candidates from follows and assign using global picker
pickRelaysForAuthors :: InboxModelEff es => [PubKeyXO] -> [RelayURI] -> Eff es (Map.Map RelayURI [PubKeyXO])
pickRelaysForAuthors pks ownInboxRelays = do
  kp <- getKeyPair
  let ownPubkey = keyPairToPubKeyXO kp

  -- Derive candidates per author from outbox lists
  relayPubkeyPairs <- forM pks $ \pk -> do
    relays <- getGeneralRelays pk
    let outboxRelayURIs = [ normalizeRelayURI (getUri r) | r <- relays, isOutboxCapable r ]
        filteredRelays = if pk == ownPubkey then outboxRelayURIs else filter (not . isLocalhostRelay) outboxRelayURIs
    -- Score/allow only acceptable relays; fallback to own inbox relays if none
    allowed <- fmap catMaybes $ forM filteredRelays $ \uri -> do
      mscore <- scoreRelay uri
      case mscore of
        Just _ -> pure (Just uri)
        Nothing -> pure Nothing
    let selected = if null allowed then ownInboxRelays else allowed
    pure (pk, selected)

  let relayToPubkeysMap = foldr
        (\(pk, relays) acc ->
          foldr (\r acc' -> Map.insertWith Set.union r (Set.singleton pk) acc') acc relays)
        Map.empty
        relayPubkeyPairs

  -- Global assignment (can evolve to multi-relay per author later)
  assignRelays (Map.map Set.toList relayToPubkeysMap)


-- | Assign relays to pubkeys given a candidate map RelayURI -> [PubKeyXO].
--   Uses per-person ranking (by relay health), sums scores to pick winner relays, and assigns
--   pubkeys whose current best relay is the winner. Respects avoid/penalties and a soft global
--   capacity that prefers already-connected relays when exceeded.
assignRelays :: InboxModelEff es => Map.Map RelayURI [PubKeyXO] -> Eff es (Map.Map RelayURI [PubKeyXO])
assignRelays candidates = do
  pool <- get @RelayPoolState
  let connectedSet :: Set.Set RelayURI
      connectedSet = Map.keysSet $ Map.filter (\rd -> connectionState rd == Connected) (activeConnections pool)

  personToRelays <- buildPersonCandidates candidates
  ranked <- rankPerPerson personToRelays
  loop connectedSet Map.empty ranked

  where
    -- Build person -> candidate relays map from relay -> [persons]
    buildPersonCandidates :: InboxModelEff es => Map.Map RelayURI [PubKeyXO] -> Eff es (Map.Map PubKeyXO (Set.Set RelayURI))
    buildPersonCandidates m = do
      let pairs = [ (pk, relay)
                  | (relay, pks) <- Map.toList m
                  , pk <- pks ]
      pure $ foldr (\(pk, r) acc -> Map.insertWith Set.union pk (Set.singleton r) acc) Map.empty pairs

    -- Rank relays per person by relay score (descending)
    rankPerPerson :: InboxModelEff es => Map.Map PubKeyXO (Set.Set RelayURI) -> Eff es (Map.Map PubKeyXO [(RelayURI, Double, Int)])
    rankPerPerson m = do
      now <- getCurrentTime
      pool <- get @RelayPoolState
      let rankOne pk rels = do
            scored <- fmap catMaybes $ mapM (scoreRelay' now pool) (Set.toList rels)
            let sorted = reverse $ sortOn (\(_, s, _) -> s) scored
            pure (pk, sorted)
      fmap Map.fromList $ mapM (\(pk, rels) -> rankOne pk rels) (Map.toList m)

    -- Score a relay; Nothing if excluded/avoided.
    scoreRelay' :: Int -> RelayPoolState -> RelayURI -> Eff es (Maybe (RelayURI, Double, Int))
    scoreRelay' now pool r = do
      case Map.lookup r (excludedRelays pool) of
        Just untilTs | untilTs > now -> pure Nothing
        _ -> case Map.lookup r (activeConnections pool) of
               -- Unknown relay: slightly optimistic default to allow exploration
               Nothing -> pure (Just (r, 0.25, 0))
               Just rd -> case avoidUntil rd of
                            Just t | t > now -> pure Nothing
                            _ -> do
                               let attempts = successCount rd + failureCount rd
                                   sr :: Double
                                   sr = if attempts <= 0 then 0.6 else fromIntegral (successCount rd) / fromIntegral attempts
                                   connectedBonus :: Double
                                   connectedBonus = if connectionState rd == Connected then 0.2 else 0.0
                                   eoseBonus :: Double
                                   eoseBonus = case lastEoseAt rd of
                                                 Just ts -> let age = max 0 (now - ts)
                                                            in if age <= 3600 then 0.2 else if age <= 86400 then 0.1 else 0.0
                                                 Nothing -> 0.0
                                   base = min 1.0 (0.6 * sr + connectedBonus + eoseBonus)
                               pure (Just (r, base, 0))

    -- Loop picking relays
    loop :: InboxModelEff es
         => Set.Set RelayURI
         -> Map.Map RelayURI [PubKeyXO]
         -> Map.Map PubKeyXO [(RelayURI, Double, Int)]
         -> Eff es (Map.Map RelayURI [PubKeyXO])
    loop connectedSet acc remainingRanks = do
      -- Build scoreboard: sum each person's current best relay score
      let bestChoices :: [(PubKeyXO, (RelayURI, Double, Int))]
          bestChoices = [ (pk, r1)
                        | (pk, rlist) <- Map.toList remainingRanks
                        , r1 : _ <- [rlist] ]
          scoreboard :: Map.Map RelayURI Double
          scoreboard = foldr (\(_, (r, s, _)) m -> Map.insertWith (+) r s m) Map.empty bestChoices

      if Map.null scoreboard
        then pure acc
        else do
          let allowed :: [(RelayURI, Double)]
              allowed = Map.toList scoreboard
          if null allowed
            then pure acc
            else do
              let (winner, _) = maximumByFst allowed
                  -- assign all persons whose current best equals the winner
                  (toAssign, remaining') = partitionByWinner winner remainingRanks
                  acc' = foldr (\pk m -> Map.insertWith (++) winner [pk] m) acc toAssign
              if null toAssign
                then pure acc -- no progress
                else loop connectedSet acc' remaining'

    maximumByFst :: Ord b => [(a,b)] -> (a,b)
    maximumByFst (x:xs) = foldr (\p acc -> if snd p > snd acc then p else acc) x xs
    maximumByFst [] = error "maximumByFst on empty list"

    partitionByWinner :: RelayURI
                      -> Map.Map PubKeyXO [(RelayURI, Double, Int)]
                      -> ([PubKeyXO], Map.Map PubKeyXO [(RelayURI, Double, Int)])
    partitionByWinner winner = Map.foldrWithKey step ([], Map.empty)
      where
        step pk ranks (as, rest) =
          case ranks of
            (r,_,_):rs | r == winner -> (pk:as, rest)
            _ -> (as, Map.insert pk ranks rest)

-- | Check if a relay URI is localhost
isLocalhostRelay :: RelayURI -> Bool
isLocalhostRelay uri =
  "127.0.0.1" `T.isInfixOf` uri ||
  "localhost" `T.isInfixOf` uri ||
  "::1" `T.isInfixOf` uri


-- | Score a relay URI for selection. Higher is better.
-- Prefers connected relays, higher success rate; excludes relays under avoidUntil.
scoreRelay :: InboxModelEff es => RelayURI -> Eff es (Maybe Double)
scoreRelay relayUri = do
  st <- get @RelayPoolState
  now <- getCurrentTime
  -- Exclude relays in penalty box (excludedRelays)
  case Map.lookup relayUri (excludedRelays st) of
    Just untilTs | untilTs > now -> pure Nothing
    _ -> case Map.lookup relayUri (activeConnections st) of
           -- Unknown relay: slightly optimistic default to allow exploration
           Nothing -> pure (Just 0.25)
           Just rd -> do
             case avoidUntil rd of
               Just t | t > now -> pure Nothing -- excluded for now
               _ -> do
                 let attempts = successCount rd + failureCount rd
                     sr :: Double
                     sr = if attempts <= 0 then 0.6 else fromIntegral (successCount rd) / fromIntegral attempts
                     connectedBonus :: Double
                     connectedBonus = if connectionState rd == Connected then 0.2 else 0.0
                     eoseBonus :: Double
                     eoseBonus = case lastEoseAt rd of
                                   Just ts -> let age = max 0 (now - ts)
                                              in if age <= 3600 then 0.2
                                                 else if age <= 86400 then 0.1
                                                 else 0.0
                                   Nothing -> 0.0
                     base = min 1.0 (0.6 * sr + connectedBonus + eoseBonus)
                 pure (Just base)


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

  -- Handle mentions subscriptions for inbox-capable relays
  void $ forConcurrently inboxRelays $ \relay' -> do
    when (isInboxCapable relay') $ do
      let relayUri = getUri relay'
      st <- get @RelayPoolState
      let isAlreadyConnected = Map.member relayUri (activeConnections st)
      connected <- if isAlreadyConnected
                    then pure True
                    else connect relayUri  -- No fallback for inbox relays
      when connected $ do
        subsForRelay <- RelayPool.getSubscriptionsForRelay relayUri
        let hasMentions = any (\(_, sd) -> isMentionsFilter (subscriptionFilter sd)) subsForRelay
        unless hasMentions $ subscribeToMentions relayUri xo

  -- Build candidates then run through picker
  candidates <- pickRelaysForAuthors followList ownInboxRelayURIs
  -- candidates already assigned by picker; skip extra filtering
  let newRelayPubkeyMap' = candidates
  -- Only update connections/subscriptions when the mapping actually changed (avoid churn)
  stPrev <- get @RelayPoolState
  let prevMap = lastRelayPubkeyMap stPrev
  when (prevMap /= newRelayPubkeyMap') $ do
    RelayPool.setLastRelayPubkeyMap newRelayPubkeyMap'

  dmRelayURIs <- getDMRelays xo
  let (defaultDMRelayURIs, _) = defaultDMRelays
      allDMRelays = Set.fromList (dmRelayURIs ++ defaultDMRelayURIs)

  st <- get @RelayPoolState
  let currentRelays = Map.keysSet (activeConnections st)
      currentGeneralRelays = Set.difference currentRelays allDMRelays
      newOutboxRelays = Map.keysSet newRelayPubkeyMap'
      newInboxRelays = Set.fromList [ normalizeRelayURI (getUri r) | r <- inboxRelays, isInboxCapable r ]
      allNeededGeneralRelays = Set.union newOutboxRelays newInboxRelays
      relaysToAdd = Set.difference newOutboxRelays currentRelays
      relaysToRemove = Set.difference currentGeneralRelays allNeededGeneralRelays
      relaysToUpdate = Set.intersection currentRelays newOutboxRelays

  -- Remove old connections
  void $ forConcurrently (Set.toList relaysToRemove) $ \relayUri -> do
    disconnect relayUri

  -- Add new connections
  void $ forConcurrently (Set.toList relaysToAdd) $ \relayUri -> do
    let pubkeys = Map.findWithDefault [] relayUri newRelayPubkeyMap'
    let pkChunks = chunksOf 20 pubkeys
    stopAllSubscriptions relayUri
    isAlreadyConnected <- RelayPool.isActiveConnection relayUri
    connected <- if isAlreadyConnected
                  then pure True
                  else connect relayUri
    if connected
      then void $ forM pkChunks $ \pks -> subscribeToProfilesAndPosts relayUri pks
      else rebalanceSubscriptions pubkeys ownInboxRelayURIs

  -- Update existing connections (only when pubkey set changed for that relay)
  void $ forConcurrently (Set.toList relaysToUpdate) $ \relayUri -> do
    let newPubkeys = Map.findWithDefault [] relayUri newRelayPubkeyMap'
    let oldPubkeys = Map.findWithDefault [] relayUri prevMap
    when (Set.fromList newPubkeys /= Set.fromList oldPubkeys) $ do
      let pkChunks = chunksOf 20 newPubkeys
      stopAllSubscriptions relayUri
      isAlreadyConnected <- RelayPool.isActiveConnection relayUri
      connected <- if isAlreadyConnected
                    then pure True
                    else connect relayUri
      if connected
        then void $ forM pkChunks $ \pks -> subscribeToProfilesAndPosts relayUri pks
        else rebalanceSubscriptions newPubkeys ownInboxRelayURIs


-- | Update DM subscriptions
updateDMSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateDMSubscriptions xo = do
    kp <- getKeyPair
    let ownPubkey = keyPairToPubKeyXO kp

    when (xo == ownPubkey) $ do
        dmRelayURIs <- getDMRelays xo

        let dmRelaySet = Set.fromList dmRelayURIs
        st <- get @RelayPoolState

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
            isAlreadyConnected <- RelayPool.isActiveConnection relayUri

            connected <- if isAlreadyConnected
                          then return True
                          else connect relayUri
            if connected
                then do
                    stopAllSubscriptions relayUri
                    subscribeToGiftwraps relayUri xo
                else pure ()
