{-# LANGUAGE BlockArguments #-}

-- | Module: Nostr.InboxModel
-- Defines the inbox model for the Nostr protocol according to NIP-65.
-- https://github.com/nostr-protocol/nips/blob/master/65.md
module Nostr.InboxModel where

import Control.Monad (forever, forM, forM_, unless, void, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, cancel, forConcurrently, forConcurrently_)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, put, modify)
--import System.CPUTime

import KeyMgmt (KeyMgmt)
import Logging (Logging, logDebug)
import Nostr
import Nostr.Event (EventId, Kind(..))
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Relay ( RelayConnection, RelayPool(..), SubscriptionState(..)
                   , connect, disconnect, subscribe, subscribeTemporary
                   , unsubscribe, unsubscribeAll, initialRelayPool, waitForCompletion )
import Nostr.Types ( Filter(..), RelayURI, SubscriptionId, getUri, topologyFilter, emptyFilter
                   , defaultGeneralRelays, commentsFilter, giftWrapFilter, mentionsFilter
                   , isInboxCapable, isOutboxCapable)
import Nostr.Util
import QtQuick (QtQuick, UIUpdates(..), emptyUpdates, notify)
import Nostr.EventProcessor (handleEvent)
import RelayMgmt
import Store.Lmdb ( LmdbStore, RelayStats(..), emptyRelayStats, getFollows, getGeneralRelays
                  , getDMRelays, getRelayStatsMany )
import Types (AppState(..), Follow(..), InboxModelState(..))

-- | InboxModel effects
data InboxModel :: Effect where
  StartInboxModel :: InboxModel m ()
  StopInboxModel :: InboxModel m ()
  SubscribeToProfilesAndPostsFor :: PubKeyXO -> InboxModel m [SubscriptionId]
  SubscribeToCommentsFor :: EventId -> InboxModel m ()
  UnsubscribeToCommentsFor :: EventId -> InboxModel m ()

type instance DispatchOf InboxModel = Dynamic

type InboxModelEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
  , KeyMgmt :> es
  , RelayConnection :> es
  , RelayMgmt :> es
  , Concurrent :> es
  , Util :> es
  , Logging :> es
  , Nostr :> es
  , QtQuick :> es
  , IOE :> es
  )

startInboxModel :: InboxModel :> es => Eff es ()
startInboxModel = send StartInboxModel

stopInboxModel :: InboxModel :> es => Eff es ()
stopInboxModel = send StopInboxModel

subscribeToProfilesAndPostsFor :: InboxModel :> es => PubKeyXO -> Eff es [SubscriptionId]
subscribeToProfilesAndPostsFor xo = send $ SubscribeToProfilesAndPostsFor xo

subscribeToCommentsFor :: InboxModel :> es => EventId -> Eff es ()
subscribeToCommentsFor eid = send $ SubscribeToCommentsFor eid

unsubscribeToCommentsFor :: InboxModel :> es => EventId -> Eff es ()
unsubscribeToCommentsFor eid = send $ UnsubscribeToCommentsFor eid


-- | Run InboxModel
runInboxModel :: InboxModelEff es => Eff (InboxModel : es) a -> Eff es a
runInboxModel = interpret $ \_ -> \case
  StartInboxModel -> do
    setInitialBootstrap
    xo <- keyPairToPubKeyXO <$> getKeyPair
    inboxRelays <- getGeneralRelays xo
    dmRelays <- getDMRelays xo
    modify @AppState $ \st -> st
      { currentDMRelays = Map.fromList $ zip dmRelays (repeat ())
      , currentGeneralRelays = Map.fromList $ zip (map getUri inboxRelays) inboxRelays
      }

    connectedRelays <- connectRelays inboxRelays

    -- Possibly bootstrap inbox model
    when (null inboxRelays) $ do
      logDebug $ "Bootstrapping inbox model"
      forConcurrently_ connectedRelays $ \r -> do
          subId <- subscribeTemporary (getUri r) (topologyFilter [xo]) handleEvent
          logDebug $ "Subscribed to relay: " <> getUri r <> " " <> pack (show subId)
          waitForCompletion subId
          logDebug $ "Completed subscription to relay: " <> getUri r <> " " <> pack (show subId)

      follows <- getFollows xo
      unless (null follows) $ forConcurrently_ connectedRelays $ \r -> do
          subId <- subscribeTemporary (getUri r) (topologyFilter $ map pubkey follows) handleEvent
          waitForCompletion subId

      myGeneralRelays <- getGeneralRelays xo
      myDMRelays <- getDMRelays xo

      when (null myGeneralRelays) $ setDefaultGeneralRelays
      when (null myDMRelays) $ setDefaultDMRelays

    -- Subscribe to giftwraps on DM relays
    let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]
    st <- get @AppState
    let dmRelayURIs = Map.keys $ currentDMRelays st
    forM_ dmRelays $ \relayUri -> async $ do
      connected <- connect relayUri
      when connected $ void $ subscribe relayUri (giftWrapFilter xo) handleEvent

    -- Subscribe to profiles and posts on general relays
    follows <- getFollows xo
    let followList = xo : map pubkey follows
    followRelayMap <- buildRelayPubkeyMap followList ownInboxRelayURIs
    forM_ (Map.toList followRelayMap) $ \(relayUri, pks) -> async $ do
      connected <- connect relayUri
      when connected $ void $ subscribe relayUri (topologyFilter pks) handleEvent

    setLiveProcessing

    -- Start a reconciliation thread that periodically refreshes connections and subscriptions.
    -- It handles:
    --   1. Follow list changes (authors added/removed)
    --   2. Relay preference changes (general and DM relays)
    --   3. Any other metadata affecting subscription topology
    --
    -- The thread runs every 15 seconds and keeps attempting
    -- reconnection/alternative selection and subscription reconciliation.
    reconciliationThread' <- async $ forever $ do
        threadDelay 15000000
        --a <- liftIO $ getCPUTime
        --reconcileSubscriptions xo
        --b <- liftIO $ getCPUTime
        --let diff = (fromIntegral (b - a)) / (10^(12::Int)) :: Double
        --logDebug $ "Reconciliation took " <> pack (show diff) <> "sec"
    modify @RelayPool $ \s -> s { reconciliationThread = Just reconciliationThread' }
    where
      setInitialBootstrap = do
          modify @AppState $ \s -> s { inboxModelState = InitialBootstrap }
          notify $ emptyUpdates { inboxModelStateChanged = True }
      setLiveProcessing = do
          modify @AppState $ \s -> s { inboxModelState = LiveProcessing }
          notify $ emptyUpdates { inboxModelStateChanged = True }
      connectRelays rs = do
        let (defaultRelays, _) = defaultGeneralRelays
            rs' = if null rs then defaultRelays else rs
        connectionResults <- forConcurrently rs' $ \r -> do
          connected <- connect $ getUri r
          --logDebug $ "Connected to relay: " <> getUri r <> " " <> pack (show connected)
          pure (r, connected)
        pure [ r | (r, True) <- connectionResults ]

  StopInboxModel -> do
    st <- get @RelayPool
    forM_ (reconciliationThread st) cancel
    forM_ (Map.keys $ activeConnections st) $ \relayUri -> do
        unsubscribeAll relayUri
        disconnect relayUri
    put @RelayPool initialRelayPool
    modify @AppState $ \s -> s { inboxModelState = Stopped }
    notify $ emptyUpdates { inboxModelStateChanged = True }

  SubscribeToProfilesAndPostsFor xo -> do
      --logDebug $ "SubscribeToProfilesAndPostsFor: " <> pack (show xo)
      myXO <- keyPairToPubKeyXO <$> getKeyPair
      st <- get @AppState
      let ownInboxRelayURIs = Map.keys $ currentGeneralRelays st
      followRelayMap <- buildRelayPubkeyMap [xo] ownInboxRelayURIs

      connectionResults <- forConcurrently (Map.toList followRelayMap) $ \(relayUri, _) -> do
          isConnected <- connect relayUri
          return (relayUri, isConnected)

      allSubIds <- fmap concat $ forM connectionResults $ \(relayUri, isConnected) -> do
          if isConnected
              then do
                  let pubkeys = fromMaybe [] (Map.lookup relayUri followRelayMap)
                  subIdTopology <- subscribe relayUri (topologyFilter pubkeys) handleEvent
                  let postsFilter = emptyFilter { authors = Just pubkeys, kinds = Just [ShortTextNote, Repost], limit = Just 100 }
                  subIdPosts <- subscribe relayUri postsFilter handleEvent
                  return [subIdTopology, subIdPosts]
              else return []

      return allSubIds

  SubscribeToCommentsFor eid -> do
    pool <- get @RelayPool
    let activeRelays = Map.keys $ activeConnections pool

    forM_ activeRelays $ \relayUri -> do
      subId' <- subscribe relayUri (commentsFilter eid) handleEvent
      modify @RelayPool $ \s -> s { commentSubscriptions = Map.insertWith (++) eid [subId'] (commentSubscriptions s) }

  UnsubscribeToCommentsFor eid -> do
    pool <- get @RelayPool
    let subIds = Map.findWithDefault [] eid (commentSubscriptions pool)

    forM_ subIds unsubscribe

    modify @RelayPool $ \s ->
      s { commentSubscriptions = Map.delete eid (commentSubscriptions s) }


-- | Compute since from relay stats (last EOSE) and local subscription state (latestCreatedAtSeen)
computeSinceForRelay :: InboxModelEff es => RelayURI -> Eff es (Maybe Int)
computeSinceForRelay relayUri = do
  st <- get @RelayPool
  let localMax =
        case [ latestCreatedAtSeen sd | (_, sd) <- Map.toList (subscriptions st), relay sd == relayUri ] of
          [] -> 0
          xs -> maximum xs
  statsMap <- getRelayStatsMany [relayUri]
  let lastEose = maybe 0 lastEoseTs (Map.lookup relayUri statsMap)
      base = max localMax lastEose
  if base > 0 then pure (Just base) else pure Nothing


-- | Compute since for giftwraps only (with 2-day overlap)
computeSinceForGiftwrap :: InboxModelEff es => RelayURI -> Eff es (Maybe Int)
computeSinceForGiftwrap relayUri = do
  base <- computeSinceForRelay relayUri
  pure $ fmap (\ts -> ts - 2 * 24 * 60 * 60) base


-- | Apply since to a filter
applySince :: Filter -> Maybe Int -> Filter
applySince f s = f { since = s }


-- | Disconnect relay if there are no active subscriptions bound to it
disconnectIfUnused :: InboxModelEff es => RelayURI -> Eff es ()
disconnectIfUnused relayUri = do
  st <- get @RelayPool
  let hasSubs = any (\(_, sd) -> relay sd == relayUri) (Map.toList (subscriptions st))
  unless hasSubs $ disconnect relayUri


-- | Build a map from relay URI to pubkeys, using inbox relays only as fallback
buildRelayPubkeyMap :: InboxModelEff es => [PubKeyXO] -> [RelayURI] -> Eff es (Map.Map RelayURI [PubKeyXO])
buildRelayPubkeyMap pks ownInboxRelays = do
  ownPubkey <- keyPairToPubKeyXO <$> getKeyPair
  now       <- getCurrentTime
  pool      <- get @RelayPool

  -- Collect candidate relays for each pubkey (with fallback), and batch-fetch stats once
  candidateLists <- forM pks $ \pk -> do
    relays <- getGeneralRelays pk
    let outboxRelayURIs = [getUri r | r <- relays, isOutboxCapable r]
        candidates = if pk == ownPubkey then outboxRelayURIs else filter (not . isLocalhostRelay) outboxRelayURIs
    pure (pk, if null candidates then ownInboxRelays else candidates)

  let allCandidates = Set.toList $ Set.fromList $ concat [ cs | (_, cs) <- candidateLists ]
  statsMap <- getRelayStatsMany allCandidates

  let score u =
        let s = Map.findWithDefault emptyRelayStats u statsMap
            succs  = fromIntegral (successes s) :: Double
            errs   = fromIntegral (errorsCount s) :: Double
            total  = succs + errs
            rate   = if total == 0 then 0 else succs / total
            nowFail = lastFailureTs s > 0 && (now - lastFailureTs s) < (10 * 60)
        in  (0.5 + 0.5 * rate)
          * (if succs > 0 then 1 + 0.5 * logBase 10 (1 + succs) else 1)
          * (if lastEoseTs s > 0 then 1.1 else 1)
          * (if Map.member u (activeConnections pool) then 1.2 else 1)
          * (if nowFail then 0.1 else 1)

      bestFor (pk, cs) = case cs of
        [] -> (pk, Nothing)
        xs -> let rBest = fst $ List.maximumBy (comparing snd) [(u, score u) | u <- xs]
              in (pk, Just rBest)

      relayPubkeyPairs = map bestFor candidateLists

  pure $ Map.fromListWith (++) [(r, [pk]) | (pk, Just r) <- relayPubkeyPairs]


-- | Check if a relay URI is localhost
isLocalhostRelay :: RelayURI -> Bool
isLocalhostRelay uri =
  "127.0.0.1" `T.isInfixOf` uri ||
  "localhost" `T.isInfixOf` uri ||
  "::1" `T.isInfixOf` uri


-- | Unified reconciliation for general and DM subscriptions
reconcileSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
reconcileSubscriptions xo = do
  -- Timeout stalled subscriptions (no EOSE)
  now <- getCurrentTime
  stTimeout <- get @RelayPool
  forM_ (Map.keys (subscriptions stTimeout)) $ \sid -> do
    st' <- get @RelayPool
    forM_ (Map.lookup sid (subscriptions st')) $ \sd -> do
      let baseTimeout = 25 -- seconds
          inactivityGrace = if lastActivityTs sd > 0 then 25 else 0  -- extra grace after first activity
          anchor = if lastActivityTs sd > 0 then lastActivityTs sd else startedAt sd
          deadline = anchor + baseTimeout + (if lastActivityTs sd > 0 then inactivityGrace else 0)
      -- Only timeout temporary (one-shot) subscriptions; long-lived streams may not EOSE quickly on some relays
      when (isTemporary sd && eventsSeen sd == 0 && not (eoseSeen sd) && now > deadline) $ do
        logDebug $ "Reconcile: Unsubscribe dead temporary subscription: " <> relay sd <> " sid=" <> sid
        unsubscribe sid

  follows <- getFollows xo
  let followList = xo : map pubkey follows
  st <- get @AppState
  let inboxRelays = Map.elems $ currentGeneralRelays st
      ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]
  newRelayPubkeyMap <- buildRelayPubkeyMap followList ownInboxRelayURIs
  st <- get @AppState
  let dmRelayURIs = Map.keys $ currentDMRelays st
  let dmRelaySet = Set.fromList dmRelayURIs
  st <- get @RelayPool

  let currentRelays   = Map.keysSet (activeConnections st)
      requiredRelays  = Set.unions [ Map.keysSet newRelayPubkeyMap
                                   , Set.fromList ownInboxRelayURIs
                                   , dmRelaySet
                                   ]
      relaysToRemove  = Set.difference currentRelays requiredRelays
      relaysToAdd     = Set.difference requiredRelays currentRelays
      relaysUnchanged = Set.intersection currentRelays requiredRelays

  remove relaysToRemove
  add relaysToAdd ownInboxRelayURIs newRelayPubkeyMap dmRelaySet
  adjust relaysUnchanged ownInboxRelayURIs newRelayPubkeyMap dmRelaySet
  where
    remove relays = forConcurrently_ (Set.toList relays) $ \r -> do
      logDebug $ "Reconcile: remove relay: " <> r
      unsubscribeAll r
      disconnect r

    add relays ownInboxURIs relayPkMap dmSet =
      forConcurrently_ (Set.toList relays) $ \r -> do
        logDebug $ "Reconcile: add relay: " <> r
        connected <- connect r
        when connected $ do
          when (r `elem` ownInboxURIs) $ do
            s <- computeSinceForRelay r
            void $ subscribe r (applySince (mentionsFilter xo) s) handleEvent
          case Map.lookup r relayPkMap of
            Just pks | not (null pks) -> do
              s <- computeSinceForRelay r
              void $ subscribe r (applySince (topologyFilter pks) s) handleEvent
            _ -> pure ()
          when (r `Set.member` dmSet) $ do
            s <- computeSinceForGiftwrap r
            void $ subscribe r (applySince (giftWrapFilter xo) s) handleEvent

    adjust relays ownInboxURIs relayPkMap dmSet =
      forConcurrently_ (Set.toList relays) $ \r -> do
        st' <- get @RelayPool

        let subsList = [ (sid, sd) | (sid, sd) <- Map.toList (subscriptions st'), relay sd == r ]
            isTopology sd = fromMaybe [] (kinds (subscriptionFilter sd)) == [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
            isMentions sd = fromMaybe [] (kinds (subscriptionFilter sd)) == [ShortTextNote, Repost, Comment, EventDeletion]
            isGift sd = GiftWrap `elem` fromMaybe [] (kinds (subscriptionFilter sd))
            currentAuthors = Set.fromList $ concat [ fromMaybe [] (authors (subscriptionFilter sd)) | (_, sd) <- subsList, isTopology sd ]
            desiredAuthors = Set.fromList $ Map.findWithDefault [] r relayPkMap
            hasMentions = any (\(_, sd) -> isMentions sd) subsList
            wantsMentions = r `elem` ownInboxURIs
            hasGiftwrap = any (\(_, sd) -> isGift sd) subsList
            authorsChanged = desiredAuthors /= currentAuthors
            mentionsAdd = wantsMentions && not hasMentions
            mentionsRemove = not wantsMentions && hasMentions
            giftAdd = r `Set.member` dmSet && not hasGiftwrap
            giftRemove = not (r `Set.member` dmSet) && hasGiftwrap

        when (authorsChanged || mentionsAdd || mentionsRemove || giftAdd || giftRemove) $ do
          logDebug $ "Reconcile: adjust relay: " <> r
          connected <- connect r
          when connected $ do
            when mentionsRemove $ do
              logDebug $ "Reconcile: mentionsRemove: " <> r
              forM_ [ sid | (sid, sd) <- subsList, isMentions sd ] unsubscribe

            when authorsChanged $ do
              logDebug $ "Reconcile: authorsChanged: " <> r
              logDebug $ "Reconcile: currentAuthors: " <> pack (show currentAuthors)
              logDebug $ "Reconcile: desiredAuthors: " <> pack (show desiredAuthors)
              forM_ [ sid | (sid, sd) <- subsList, isTopology sd ] unsubscribe

            when giftRemove $ do
              logDebug $ "Reconcile: giftRemove: " <> r
              forM_ [ sid | (sid, sd) <- subsList, isGift sd ] unsubscribe

            when mentionsAdd $ do
              logDebug $ "Reconcile: mentionsAdd: " <> r
              s <- computeSinceForRelay r
              void $ subscribe r (applySince (mentionsFilter xo) s) handleEvent

            when authorsChanged $ do
              let desiredList = Set.toList desiredAuthors
              s <- computeSinceForRelay r
              void $ subscribe r (applySince (topologyFilter desiredList) s) handleEvent

            when giftAdd $ do
              logDebug $ "Reconcile: giftAdd: " <> r
              s <- computeSinceForGiftwrap r
              void $ subscribe r (applySince (giftWrapFilter xo) s) handleEvent
