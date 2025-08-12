{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Nostr.RelayPool
  ( -- Effect
    RelayPool(..)
  , runRelayPool
    -- State model
  , RelayPoolState(..)
  , RelayData(..)
  , SubscriptionState(..)
  , RelayStats(..)
  , initialRelayPool
  , queueMax
    -- Smart constructors (API)
    --
    -- Connections
  , adjustActiveConnection
  , incrementFailure
  , incrementSuccess
  , setLastConnectedAt
  , setLastEoseAt
  , setExcludedRelayUntil
  , setPendingUserDisconnect
  , incrementMessagesReceived
  , incrementEventsReceived
  , getActiveConnectionsMap
  , insertActiveConnection
  , clearExcludedRelay
  , setAvoidUntil
  , isActiveConnection

  -- Subscriptions
  , adjustSubscription
  , upsertPublishStatus
  , setPendingAuthId
  , setSubEoseReceived
  , deleteSubscriptionId
  , mergeSubscriptions
  , setPendingSubscriptions
  , deleteSubscriptionsBulk
  , filterPendingSubscriptionsByRelay
  , insertSubscription
  , insertPendingSubscription
  , listSubscriptionIdsForRelay
  , getSubscriptionsForRelay
  , setSubscriptionLastSentFilter

  -- Comments (per-event subscriptions)
  , insertCommentSubscription
  , deleteCommentSubscription

  -- Pool administration
  , reset

  -- Background threads / snapshots / topology
  , setUpdateThread
  , setStatsThread
  , setReconcileThread
  , setLastStatsSnapshot
  , setLastRelayPubkeyMap
  ) where

import Effectful
import Effectful.Dispatch.Dynamic (send, interpret)
import Effectful.State.Static.Shared (State, get, modify)

import Data.Map.Strict qualified as Map
import Control.Concurrent.Async (Async)
import Effectful.Concurrent.STM (TChan, TQueue)

import Nostr.Relay (RelayURI)
import Types ( ConnectionState(..), PublishStatus(..) )
import Nostr.Event (EventId)
import Nostr.Types (Filter)
import Nostr.Types qualified as NT
import Nostr.Keys (PubKeyXO)
import Nostr.Event (Event)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- Effect for RelayPool high-level operations
data RelayPool :: Effect where
  AdjustActiveConnection :: RelayURI -> (RelayData -> RelayData) -> RelayPool m ()
  IncrementFailure :: RelayURI -> RelayPool m ()
  IncrementSuccess :: RelayURI -> RelayPool m ()
  SetLastConnectedAt :: RelayURI -> Int -> RelayPool m ()
  SetLastEoseAt :: RelayURI -> Int -> RelayPool m ()
  SetExcludedRelayUntil :: RelayURI -> Int -> RelayPool m ()
  SetPendingUserDisconnect :: RelayURI -> Bool -> RelayPool m ()
  IncrementMessagesReceived :: RelayURI -> RelayPool m ()
  IncrementEventsReceived :: RelayURI -> RelayPool m ()

  AdjustSubscription :: NT.SubscriptionId -> (SubscriptionState -> SubscriptionState) -> RelayPool m ()
  UpsertPublishStatus :: EventId -> RelayURI -> PublishStatus -> RelayPool m ()
  SetPendingAuthId :: RelayURI -> Maybe EventId -> RelayPool m ()
  SetSubEoseReceived :: NT.SubscriptionId -> RelayPool m ()
  DeleteSubscriptionId :: NT.SubscriptionId -> RelayPool m ()
  MergeSubscriptions :: Map.Map NT.SubscriptionId SubscriptionState -> RelayPool m ()
  SetPendingSubscriptions :: Map.Map NT.SubscriptionId SubscriptionState -> RelayPool m ()
  GetActiveConnectionsMap :: RelayPool m (Map.Map RelayURI RelayData)
  InsertActiveConnection :: RelayURI -> RelayData -> RelayPool m ()
  ClearExcludedRelay :: RelayURI -> RelayPool m ()
  SetAvoidUntil :: RelayURI -> Int -> RelayPool m ()
  DeleteSubscriptionsBulk :: [NT.SubscriptionId] -> RelayPool m ()
  FilterPendingSubscriptionsByRelay :: RelayURI -> RelayPool m ()
  InsertSubscription :: NT.SubscriptionId -> SubscriptionState -> RelayPool m ()
  InsertPendingSubscription :: NT.SubscriptionId -> SubscriptionState -> RelayPool m ()
  Reset :: RelayPool m ()
  ListSubscriptionIdsForRelay :: RelayURI -> RelayPool m [NT.SubscriptionId]
  GetSubscriptionsForRelay :: RelayURI -> RelayPool m [(NT.SubscriptionId, SubscriptionState)]
  IsActiveConnection :: RelayURI -> RelayPool m Bool
  SetUpdateThread :: Maybe (Async ()) -> RelayPool m ()
  SetStatsThread :: Maybe (Async ()) -> RelayPool m ()
  SetReconcileThread :: Maybe (Async ()) -> RelayPool m ()
  SetLastStatsSnapshot :: Map.Map RelayURI (Int, Int) -> RelayPool m ()
  SetSubscriptionLastSentFilter :: NT.SubscriptionId -> Filter -> RelayPool m ()
  SetLastRelayPubkeyMap :: Map.Map RelayURI [PubKeyXO] -> RelayPool m ()
  InsertCommentSubscription :: EventId -> NT.SubscriptionId -> RelayPool m ()
  DeleteCommentSubscription :: EventId -> RelayPool m ()

type instance DispatchOf RelayPool = Dynamic

-- | Global cap for request/event queues per relay
queueMax :: Int
queueMax = 200

-- | Persisted relay statistics
data RelayStats = RelayStats
  { rsSuccessCount :: Int
  , rsFailureCount :: Int
  , rsLastConnectedAt :: Maybe Int
  , rsLastEoseAt :: Maybe Int
  , rsAvoidUntil :: Maybe Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


-- | Subscription details stored in the pool
data SubscriptionState = SubscriptionState
    { subscriptionFilter :: Filter
    , relay :: RelayURI
    , eventsProcessed :: Int
    , oldestCreatedAt :: Int
    , eoseReceived :: Bool
    , temporary :: Bool
    , lastSentFilter :: Maybe Filter
    } deriving (Eq, Show)

newSubscriptionState :: Filter -> RelayURI -> Bool -> SubscriptionState
newSubscriptionState f r isTemp = SubscriptionState f r 0 (maxBound :: Int) False isTemp (Just f)

-- | Data for each relay connection
data RelayData = RelayData
  { connectionState :: ConnectionState
  , requestChannel :: TChan NT.Request
  , notices        :: [Text]
  , lastError      :: Maybe Text
  , connectionAttempts :: Int
  , pendingRequests :: [NT.Request]
  , pendingEvents :: [Event]
  , queuedRequests :: [NT.Request]
  , pendingAuthId :: Maybe EventId
  , recentFailure :: Bool
  , successCount :: Int
  , failureCount :: Int
  , lastConnectedAt :: Maybe Int
  , lastEoseAt :: Maybe Int
  , messagesReceived :: Int
  , eventsReceived :: Int
  , authRequiredCount :: Int
  , noticeCount :: Int
  , avoidUntil :: Maybe Int
  , pendingUserDisconnect :: Bool
  } deriving (Eq)

-- | Entire relay pool model
data RelayPoolState = RelayPoolState
    { activeConnections :: Map.Map RelayURI RelayData
    , subscriptions :: Map.Map NT.SubscriptionId SubscriptionState
    , pendingSubscriptions :: Map.Map NT.SubscriptionId SubscriptionState
    , publishStatus :: Map.Map EventId (Map.Map RelayURI Types.PublishStatus)
    , updateQueue :: TQueue ()
    , updateThread :: Maybe (Async ())
    , statsThread :: Maybe (Async ())
    , reconcileThread :: Maybe (Async ())
    , commentSubscriptions :: Map.Map EventId [NT.SubscriptionId]
    , lastRelayPubkeyMap :: Map.Map RelayURI [PubKeyXO]
    , lastStatsSnapshot :: Map.Map RelayURI (Int, Int)
    , lastStatsSnapshotAt :: Maybe Int
    , excludedRelays :: Map.Map RelayURI Int
    , avoidSpamOnUnsafeRelays :: Bool
    }

-- | Initial state for the pool
initialRelayPool :: RelayPoolState
initialRelayPool = RelayPoolState
  { activeConnections = Map.empty
  , subscriptions = Map.empty
  , pendingSubscriptions = Map.empty
  , publishStatus = Map.empty
  , updateQueue = undefined
  , updateThread = Nothing
  , statsThread = Nothing
  , reconcileThread = Nothing
  , commentSubscriptions = Map.empty
  , lastRelayPubkeyMap = Map.empty
  , lastStatsSnapshot = Map.empty
  , lastStatsSnapshotAt = Nothing
  , excludedRelays = Map.empty
  , avoidSpamOnUnsafeRelays = False
  }

-- API helpers
adjustActiveConnection :: RelayPool :> es => RelayURI -> (RelayData -> RelayData) -> Eff es ()
adjustActiveConnection uri f = send (AdjustActiveConnection uri f)

-- setConnectionState removed from API; callers should use adjustActiveConnection

incrementFailure :: RelayPool :> es => RelayURI -> Eff es ()
incrementFailure uri = send (IncrementFailure uri)

incrementSuccess :: RelayPool :> es => RelayURI -> Eff es ()
incrementSuccess uri = send (IncrementSuccess uri)

setLastConnectedAt :: RelayPool :> es => RelayURI -> Int -> Eff es ()
setLastConnectedAt uri ts = send (SetLastConnectedAt uri ts)

setLastEoseAt :: RelayPool :> es => RelayURI -> Int -> Eff es ()
setLastEoseAt uri ts = send (SetLastEoseAt uri ts)

setExcludedRelayUntil :: RelayPool :> es => RelayURI -> Int -> Eff es ()
setExcludedRelayUntil uri ts = send (SetExcludedRelayUntil uri ts)

setPendingUserDisconnect :: RelayPool :> es => RelayURI -> Bool -> Eff es ()
setPendingUserDisconnect uri b = send (SetPendingUserDisconnect uri b)

incrementMessagesReceived :: RelayPool :> es => RelayURI -> Eff es ()
incrementMessagesReceived uri = send (IncrementMessagesReceived uri)

incrementEventsReceived :: RelayPool :> es => RelayURI -> Eff es ()
incrementEventsReceived uri = send (IncrementEventsReceived uri)

adjustSubscription :: RelayPool :> es => NT.SubscriptionId -> (SubscriptionState -> SubscriptionState) -> Eff es ()
adjustSubscription sid f = send (AdjustSubscription sid f)

-- deleteSubscription removed; use deleteSubscriptionId

upsertPublishStatus :: RelayPool :> es => EventId -> RelayURI -> PublishStatus -> Eff es ()
upsertPublishStatus eid uri status = send (UpsertPublishStatus eid uri status)

setPendingAuthId :: RelayPool :> es => RelayURI -> Maybe EventId -> Eff es ()
setPendingAuthId uri mid = send (SetPendingAuthId uri mid)

setSubEoseReceived :: RelayPool :> es => NT.SubscriptionId -> Eff es ()
setSubEoseReceived sid = send (SetSubEoseReceived sid)

-- removed deprecated setLastEoseTimestamp; use setLastEoseAt

deleteSubscriptionId :: RelayPool :> es => NT.SubscriptionId -> Eff es ()
deleteSubscriptionId sid = send (DeleteSubscriptionId sid)

mergeSubscriptions :: RelayPool :> es => Map.Map NT.SubscriptionId SubscriptionState -> Eff es ()
mergeSubscriptions m = send (MergeSubscriptions m)

setPendingSubscriptions :: RelayPool :> es => Map.Map NT.SubscriptionId SubscriptionState -> Eff es ()
setPendingSubscriptions m = send (SetPendingSubscriptions m)

getActiveConnectionsMap :: RelayPool :> es => Eff es (Map.Map RelayURI RelayData)
getActiveConnectionsMap = send GetActiveConnectionsMap

insertActiveConnection :: RelayPool :> es => RelayURI -> RelayData -> Eff es ()
insertActiveConnection uri rd = send (InsertActiveConnection uri rd)

clearExcludedRelay :: RelayPool :> es => RelayURI -> Eff es ()
clearExcludedRelay uri = send (ClearExcludedRelay uri)

setAvoidUntil :: RelayPool :> es => RelayURI -> Int -> Eff es ()
setAvoidUntil uri ts = send (SetAvoidUntil uri ts)

deleteSubscriptionsBulk :: RelayPool :> es => [NT.SubscriptionId] -> Eff es ()
deleteSubscriptionsBulk sids = send (DeleteSubscriptionsBulk sids)

filterPendingSubscriptionsByRelay :: RelayPool :> es => RelayURI -> Eff es ()
filterPendingSubscriptionsByRelay uri = send (FilterPendingSubscriptionsByRelay uri)

insertSubscription :: RelayPool :> es => NT.SubscriptionId -> SubscriptionState -> Eff es ()
insertSubscription sid sub = send (InsertSubscription sid sub)

insertPendingSubscription :: RelayPool :> es => NT.SubscriptionId -> SubscriptionState -> Eff es ()
insertPendingSubscription sid sub = send (InsertPendingSubscription sid sub)

reset :: RelayPool :> es => Eff es ()
reset = send Reset

listSubscriptionIdsForRelay :: RelayPool :> es => RelayURI -> Eff es [NT.SubscriptionId]
listSubscriptionIdsForRelay uri = send (ListSubscriptionIdsForRelay uri)

getSubscriptionsForRelay :: RelayPool :> es => RelayURI -> Eff es [(NT.SubscriptionId, SubscriptionState)]
getSubscriptionsForRelay uri = send (GetSubscriptionsForRelay uri)

isActiveConnection :: RelayPool :> es => RelayURI -> Eff es Bool
isActiveConnection uri = send (IsActiveConnection uri)

-- Threads / snapshots / topology
setUpdateThread :: RelayPool :> es => Maybe (Async ()) -> Eff es ()
setUpdateThread t = send (SetUpdateThread t)

setStatsThread :: RelayPool :> es => Maybe (Async ()) -> Eff es ()
setStatsThread t = send (SetStatsThread t)

setReconcileThread :: RelayPool :> es => Maybe (Async ()) -> Eff es ()
setReconcileThread t = send (SetReconcileThread t)

setLastStatsSnapshot :: RelayPool :> es => Map.Map RelayURI (Int, Int) -> Eff es ()
setLastStatsSnapshot m = send (SetLastStatsSnapshot m)

setSubscriptionLastSentFilter :: RelayPool :> es => NT.SubscriptionId -> Filter -> Eff es ()
setSubscriptionLastSentFilter sid f = send (SetSubscriptionLastSentFilter sid f)

setLastRelayPubkeyMap :: RelayPool :> es => Map.Map RelayURI [PubKeyXO] -> Eff es ()
setLastRelayPubkeyMap m = send (SetLastRelayPubkeyMap m)

insertCommentSubscription :: RelayPool :> es => EventId -> NT.SubscriptionId -> Eff es ()
insertCommentSubscription eid sid = send (InsertCommentSubscription eid sid)

deleteCommentSubscription :: RelayPool :> es => EventId -> Eff es ()
deleteCommentSubscription eid = send (DeleteCommentSubscription eid)

-- Interpreter backed by State TP.RelayPool
runRelayPool :: State RelayPoolState :> es => Eff (RelayPool : es) a -> Eff es a
runRelayPool = interpret $ \_ -> \case

  AdjustActiveConnection uri f ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust f uri (activeConnections st) }


  IncrementFailure uri ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { failureCount = failureCount d + 1 }) uri (activeConnections st) }

  IncrementSuccess uri ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { successCount = successCount d + 1 }) uri (activeConnections st) }

  SetLastConnectedAt uri ts ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { lastConnectedAt = Just ts }) uri (activeConnections st) }

  SetLastEoseAt uri ts ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { lastEoseAt = Just ts }) uri (activeConnections st) }

  SetExcludedRelayUntil uri untilTs ->
    modify @RelayPoolState $ \st -> st { excludedRelays = Map.insert uri untilTs (excludedRelays st) }

  SetPendingUserDisconnect uri b ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { pendingUserDisconnect = b }) uri (activeConnections st) }

  IncrementMessagesReceived uri ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { messagesReceived = messagesReceived d + 1 }) uri (activeConnections st) }

  IncrementEventsReceived uri ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { eventsReceived = eventsReceived d + 1 }) uri (activeConnections st) }


  AdjustSubscription sid f ->
    modify @RelayPoolState $ \st -> st { subscriptions = Map.adjust f sid (subscriptions st) }

  UpsertPublishStatus eid uri status ->
    modify @RelayPoolState $ \st ->
      let alterInner = \case
            Nothing -> Just (Map.singleton uri status)
            Just inner -> Just (Map.insert uri status inner)
      in st { publishStatus = Map.alter alterInner eid (publishStatus st) }

  SetPendingAuthId uri mid ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { pendingAuthId = mid }) uri (activeConnections st) }

  SetSubEoseReceived sid ->
    modify @RelayPoolState $ \st -> st { subscriptions = Map.adjust (\sd -> sd { eoseReceived = True }) sid (subscriptions st) }


  DeleteSubscriptionId sid ->
    modify @RelayPoolState $ \st -> st { subscriptions = Map.delete sid (subscriptions st) }

  MergeSubscriptions m ->
    modify @RelayPoolState $ \st -> st { subscriptions = Map.union (subscriptions st) m }

  SetPendingSubscriptions m ->
    modify @RelayPoolState $ \st -> st { pendingSubscriptions = m }

  GetActiveConnectionsMap -> do
    st <- get @RelayPoolState
    pure (activeConnections st)

  InsertActiveConnection uri rd ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.insert uri rd (activeConnections st) }

  ClearExcludedRelay uri ->
    modify @RelayPoolState $ \st -> st { excludedRelays = Map.delete uri (excludedRelays st) }

  SetAvoidUntil uri ts ->
    modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust (\d -> d { avoidUntil = Just ts }) uri (activeConnections st) }

  DeleteSubscriptionsBulk sids ->
    modify @RelayPoolState $ \st -> st { subscriptions = foldr Map.delete (subscriptions st) sids }

  FilterPendingSubscriptionsByRelay uri ->
    modify @RelayPoolState $ \st -> st { pendingSubscriptions = Map.filter (\v -> relay v /= uri) (pendingSubscriptions st) }

  InsertSubscription sid sub ->
    modify @RelayPoolState $ \st -> st { subscriptions = Map.insert sid sub (subscriptions st) }

  InsertPendingSubscription sid sub ->
    modify @RelayPoolState $ \st -> st { pendingSubscriptions = Map.insert sid sub (pendingSubscriptions st) }

  Reset ->
    modify @RelayPoolState $ const initialRelayPool

  ListSubscriptionIdsForRelay uri -> do
    st <- get @RelayPoolState
    pure [ sid | (sid, sd) <- Map.toList (subscriptions st), relay sd == uri ]

  GetSubscriptionsForRelay uri -> do
    st <- get @RelayPoolState
    pure [ (sid, sd) | (sid, sd) <- Map.toList (subscriptions st), relay sd == uri ]

  IsActiveConnection uri -> do
    st <- get @RelayPoolState
    pure (Map.member uri (activeConnections st))

  -- Threads / snapshots / topology
  SetUpdateThread mt ->
    modify @RelayPoolState $ \st -> st { updateThread = mt }

  SetStatsThread mt ->
    modify @RelayPoolState $ \st -> st { statsThread = mt }

  SetReconcileThread mt ->
    modify @RelayPoolState $ \st -> st { reconcileThread = mt }

  SetLastStatsSnapshot snap ->
    modify @RelayPoolState $ \st -> st { lastStatsSnapshot = snap }

  SetSubscriptionLastSentFilter sid f ->
    modify @RelayPoolState $ \st -> st { subscriptions = Map.adjust (\sd -> sd { lastSentFilter = Just f }) sid (subscriptions st) }

  SetLastRelayPubkeyMap m ->
    modify @RelayPoolState $ \st -> st { lastRelayPubkeyMap = m }

  InsertCommentSubscription eid sid ->
    modify @RelayPoolState $ \st ->
      let updated = Map.insertWith (++) eid [sid] (commentSubscriptions st)
      in st { commentSubscriptions = updated }

  DeleteCommentSubscription eid ->
    modify @RelayPoolState $ \st -> st { commentSubscriptions = Map.delete eid (commentSubscriptions st) }
