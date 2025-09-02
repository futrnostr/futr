{-# LANGUAGE BlockArguments #-}

-- | Module: Nostr.InboxModel
-- Defines the inbox model for the Nostr protocol according to NIP-65.
-- https://github.com/nostr-protocol/nips/blob/master/65.md
module Nostr.InboxModel where

import Control.Monad (forever, forM, forM_, unless, void, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, cancel, forConcurrently, forConcurrently_)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, put, modify)

import KeyMgmt (KeyMgmt)
import Nostr
import Nostr.Event (EventId, Kind(..))
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Relay ( RelayConnection, RelayPool(..), SubscriptionState(..)
                   , connect, disconnect, subscribe, subscribeTemporary
                   , unsubscribe, unsubscribeAll, initialRelayPool, waitForCompletion )
import Nostr.Types ( Filter(..), RelayURI, SubscriptionId, getUri, topologyFilter
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
  ConnectAndBootstrap :: InboxModel m (Either Text ())
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
  , Nostr :> es
  , QtQuick :> es
  )


connectAndBootstrap :: InboxModel :> es => Eff es (Either Text ())
connectAndBootstrap = send ConnectAndBootstrap

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
  ConnectAndBootstrap -> do
    setInitialBootstrap
    xo <- keyPairToPubKeyXO <$> getKeyPair
    inboxRelays <- getGeneralRelays xo

    connectedRelays <- connectRelays inboxRelays

    if null connectedRelays
    then pure $ Left "Connection failed"
    else do
        when (null inboxRelays) $ do
            forConcurrently_ connectedRelays $ \r -> do
                subId <- subscribeTemporary (getUri r) (topologyFilter [xo]) handleEvent
                waitForCompletion subId

            follows <- getFollows xo
            unless (null follows) $ forConcurrently_ connectedRelays $ \r -> do
                subId <- subscribeTemporary (getUri r) (topologyFilter $ map pubkey follows) handleEvent
                waitForCompletion subId

            myGeneralRelays <- getGeneralRelays xo
            myDMRelays <- getDMRelays xo

            when (null myGeneralRelays) $ setDefaultGeneralRelays
            when (null myDMRelays) $ setDefaultDMRelays
        setLiveProcessing
        pure $ Right ()
    where
        setInitialBootstrap = do
            modify @AppState $ \s -> s { inboxModelState = InitialBootstrap }
            notify $ emptyUpdates { inboxModelStateChanged = True }
        connectRelays rs = do
          let (defaultRelays, _) = defaultGeneralRelays
              rs' = if null rs then defaultRelays else rs
          connectionResults <- forConcurrently rs' $ \r -> do
            connected <- connect $ getUri r
            pure (r, connected)
          pure [ r | (r, True) <- connectionResults ]

  StartInboxModel -> do
    xo <- keyPairToPubKeyXO <$> getKeyPair

    -- Start a reconciliation thread that periodically refreshes connections and subscriptions.
    -- It handles:
    --   1. Follow list changes (authors added/removed)
    --   2. Relay preference changes (general and DM relays)
    --   3. Any other metadata affecting subscription topology
    --
    -- The thread runs every 5 seconds and keeps attempting
    -- reconnection/alternative selection and subscription reconciliation.
    reconciliationThread' <- async $ forever $ do
        threadDelay 5000000
        reconcileSubscriptions xo
    modify @RelayPool $ \s -> s { reconciliationThread = Just reconciliationThread' }

    -- Subscribe to giftwraps on DM relays
    inboxRelays <- getGeneralRelays xo
    let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]
    dmRelays <- getDMRelays xo
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
      myXO <- keyPairToPubKeyXO <$> getKeyPair
      ownInboxRelayURIs <- map getUri <$> getGeneralRelays myXO
      followRelayMap <- buildRelayPubkeyMap [xo] ownInboxRelayURIs

      connectionResults <- forConcurrently (Map.toList followRelayMap) $ \(relayUri, _) -> do
          isConnected <- connect relayUri
          return (relayUri, isConnected)

      allSubIds <- fmap concat $ forM connectionResults $ \(relayUri, isConnected) -> do
          if isConnected
              then do
                  let pubkeys = fromMaybe [] (Map.lookup relayUri followRelayMap)
                  subId <- subscribe relayUri (topologyFilter pubkeys) handleEvent
                  return [subId]
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


-- | Set the inbox model state to live processing
setLiveProcessing :: InboxModelEff es => Eff es ()
setLiveProcessing = do
  modify @AppState $ \s -> s { inboxModelState = LiveProcessing }
  notify $ emptyUpdates { inboxModelStateChanged = True }


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
  kp <- getKeyPair
  let ownPubkey = keyPairToPubKeyXO kp

  relayPubkeyPairs <- forM pks $ \pk -> do
    relays <- getGeneralRelays pk
    let outboxRelayURIs = [ getUri r | r <- relays, isOutboxCapable r ]
        -- Filter localhost for contacts (but allow for own pubkey)
        candidateRelays = if pk == ownPubkey then outboxRelayURIs else filter (not . isLocalhostRelay) outboxRelayURIs
        fallbackRelays = if null candidateRelays then ownInboxRelays else candidateRelays

    -- Score relays and pick best
    statsMap <- getRelayStatsMany fallbackRelays
    let calc u =
          let s = Map.findWithDefault emptyRelayStats u statsMap
              r = max 0 (min 9 (rank s))
              rankMul = if r == 0 then 0.0 else (fromIntegral r / 9.0)
              succs = fromIntegral (successes s) :: Double
              errs  = fromIntegral (errorsCount s) :: Double
              total = succs + errs
              rate  = if total <= 0 then 0.0 else succs / total
              rateMul = 0.5 + 0.5 * rate
              countMul = if succs > 0 then 1.0 + 0.5 * logBase 10 (1.0 + succs) else 1.0
              freshMul = if lastEoseTs s > 0 then 1.1 else 1.0
              base = 1.0 * rankMul * rateMul * countMul * freshMul
          in base
        ranked = reverse $ List.sortOn snd [ (u, calc u) | u <- fallbackRelays ]
        bestRelays = map fst ranked

    return (pk, bestRelays)

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
  forM_ (Map.toList (subscriptions stTimeout)) $ \(sid, sd) -> do
    let baseTimeout = if isTemporary sd then 15 else 30  -- seconds
        inactivityGrace = 15  -- seconds
        -- If we have activity, extend deadline from last activity; else from start
        anchor = if lastActivityTs sd > 0 then lastActivityTs sd else startedAt sd
        deadline = anchor + baseTimeout + (if lastActivityTs sd > 0 then inactivityGrace else 0)
    when (not (eoseSeen sd) && now > deadline) $ unsubscribe sid

  follows <- getFollows xo
  let followList = xo : map pubkey follows
  inboxRelays <- getGeneralRelays xo
  let ownInboxRelayURIs = [ getUri r | r <- inboxRelays, isInboxCapable r ]
  newRelayPubkeyMap <- buildRelayPubkeyMap followList ownInboxRelayURIs
  dmRelayURIs <- getDMRelays xo
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
      unsubscribeAll r
      disconnect r

    add relays ownInboxURIs relayPkMap dmSet =
      forConcurrently_ (Set.toList relays) $ \r -> do
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
        -- Attempt to connect; proceed regardless to allow queuing
        void $ connect r

        let subsList = [ (sid, sd) | (sid, sd) <- Map.toList (subscriptions st'), relay sd == r ]
            isTopology sd = fromMaybe [] (kinds (subscriptionFilter sd)) == [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
            isMentions sd = fromMaybe [] (kinds (subscriptionFilter sd)) == [ShortTextNote, Repost, Comment, EventDeletion]
            isGift sd = GiftWrap `elem` fromMaybe [] (kinds (subscriptionFilter sd))
            currentAuthors = Set.fromList $ concat [ fromMaybe [] (authors (subscriptionFilter sd)) | (_, sd) <- subsList, isTopology sd ]
            desiredAuthors = Set.fromList $ Map.findWithDefault [] r relayPkMap
            hasMentions = any (\(_, sd) -> isMentions sd) subsList
            wantsMentions = r `elem` ownInboxURIs
            hasGiftwrap = any (\(_, sd) -> isGift sd) subsList

        when (wantsMentions && not hasMentions) $ do
          s <- computeSinceForRelay r
          void $ subscribe r (applySince (mentionsFilter xo) s) handleEvent
        when (not wantsMentions && hasMentions) $ do
          forM_ [ sid | (sid, sd) <- subsList, isMentions sd ] unsubscribe

        when (desiredAuthors /= currentAuthors) $ do
          let desiredList = Set.toList desiredAuthors
              fewer = Set.size desiredAuthors < Set.size currentAuthors
          forM_ [ sid | (sid, sd) <- subsList, isTopology sd ] unsubscribe
          s <- if fewer
                then computeSinceForRelay r
                else do
                  ms <- computeSinceForRelay r
                  pure ms
          
          void $ subscribe r (applySince (topologyFilter desiredList) s) handleEvent

        if r `Set.member` dmSet
          then when (not hasGiftwrap) $ do
                  s <- computeSinceForGiftwrap r
                  void $ subscribe r (applySince (giftWrapFilter xo) s) handleEvent
          else do
            forM_ [ sid | (sid, sd) <- subsList, isGift sd ] unsubscribe
