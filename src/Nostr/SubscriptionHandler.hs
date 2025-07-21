module Nostr.SubscriptionHandler where

import Control.Monad (forM_, unless, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (pack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue, newTVarIO, readTQueue
                                , readTVar, writeTChan, writeTVar, modifyTVar)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Prelude hiding (until)

import KeyMgmt (KeyMgmt)
import Logging
import Nostr.Event (Event(..), EventId(..))
import Nostr.EventHandler (EventHandler, handleEvent)
import Nostr.Relay (RelayURI)
import Nostr.Subscription
import Nostr.Types (Filter(..), SubscriptionId)
import Nostr.Types qualified as NT
import Nostr.Util
import QtQuick
import Store.Lmdb
import Types

-- | Subscription effects
data SubscriptionHandler :: Effect where
    HandleSubscription :: SubscriptionId -> SubscriptionHandler m ()
    HandleSubscriptionUntilEOSE :: SubscriptionId -> SubscriptionHandler m ()
    HandlePaginationSubscription :: SubscriptionId -> SubscriptionHandler m ()

type instance DispatchOf SubscriptionHandler = Dynamic


handleSubscription :: SubscriptionHandler :> es => SubscriptionId -> Eff es ()
handleSubscription subId = send $ HandleSubscription subId

handleSubscriptionUntilEOSE :: SubscriptionHandler :> es => SubscriptionId -> Eff es ()
handleSubscriptionUntilEOSE subId = send $ HandleSubscriptionUntilEOSE subId

handlePaginationSubscription :: SubscriptionHandler :> es => SubscriptionId -> Eff es ()
handlePaginationSubscription subId = send $ HandlePaginationSubscription subId


-- | SubscriptionEff
type SubscriptionHandlerEff es =
  ( Subscription :> es
  , EventHandler :> es
  , LmdbStore :> es
  , KeyMgmt :> es
  , State RelayPool :> es
  , Util :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  )

-- | Handler for subscription effects.
runSubscriptionHandler
  :: SubscriptionHandlerEff es
  => Eff (SubscriptionHandler : es) a
  -> Eff es a
runSubscriptionHandler = interpret $ \_ -> \case
    HandleSubscription subId' -> do
        subs <- gets @RelayPool subscriptions
        case Map.lookup subId' subs of
            Nothing -> pure ()
            Just sub -> do
                let q = responseQueue sub
                processQueue False subId' q

    HandleSubscriptionUntilEOSE subId' -> do
        subs <- gets @RelayPool subscriptions
        case Map.lookup subId' subs of
            Nothing -> pure ()
            Just sub -> do
                let q = responseQueue sub
                processQueue True subId' q

    HandlePaginationSubscription subId' -> do
        subs <- gets @RelayPool subscriptions
        case Map.lookup subId' subs of
            Nothing -> pure ()
            Just sub -> do
                let q = responseQueue sub
                    filter' = subscriptionFilter sub
                    relayUri' = relay sub

                st <- get @RelayPool
                case Map.lookup relayUri' (activeConnections st) of
                    Nothing -> pure ()
                    Just rd -> do
                        let liveSubId = subId' <> "_live"
                            liveFilter = filter' { until = Nothing }
                            channel = requestChannel rd

                        atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription liveSubId liveFilter)

                        let liveSub = SubscriptionState
                                { subscriptionFilter = liveFilter
                                , responseQueue = q  -- Use the same queue as the pagination subscription
                                , relay = relayUri'
                                , eventsProcessed = 0
                                , oldestCreatedAt = maxBound :: Int
                                }

                        modify @RelayPool $ \st' ->
                            st' { subscriptions = Map.insert liveSubId liveSub (subscriptions st') }

                -- Now continue with the pagination subscription as before
                shouldStopVar <- newTVarIO False
                seenEventsVar <- newTVarIO Set.empty
                currentBatchVar <- newTVarIO Set.empty
                nextUntilVar <- newTVarIO Nothing


                let loop = do
                        e  <- atomically $ readTQueue q
                        es <- atomically $ flushTQueue q
                        let events = e:es

                        seenEvents <- atomically $ readTVar seenEventsVar
                        let newEvents = filter (\(_, e') ->
                                case e' of
                                    EventAppeared evt -> not $ Set.member (getEventId $ eventId evt) seenEvents
                                    _ -> True
                                ) events

                        forM_ newEvents $ \(relayUri, e') -> case e' of
                            EventAppeared event' -> do
                                handleEvent (Just relayUri) event'
                                atomically $ do
                                    modifyTVar seenEventsVar $ Set.insert (getEventId $ eventId event')
                                    modifyTVar currentBatchVar $ Set.insert (getEventId $ eventId event')
                                    modifyTVar nextUntilVar $ \current -> case current of
                                        Nothing -> Just (createdAt event')
                                        Just t -> Just (min t (createdAt event'))

                            SubscriptionEose _ -> do
                                subs' <- gets @RelayPool subscriptions
                                case Map.lookup subId' subs' of
                                    Nothing -> pure ()
                                    Just subInfo -> do
                                        let filter'' = subscriptionFilter subInfo
                                            requestedLimit = limit filter''
                                            seenEventsCount = Set.size seenEvents

                                        currentBatch <- atomically $ readTVar currentBatchVar
                                        nextUntil <- atomically $ readTVar nextUntilVar

                                        let shouldPaginate = not (Set.null currentBatch)
                                                && maybe True (\l -> seenEventsCount < l) requestedLimit
                                                && isJust nextUntil

                                        if shouldPaginate
                                            then do
                                                st' <- get @RelayPool
                                                case Map.lookup relayUri' (activeConnections st') of
                                                    Nothing -> pure ()
                                                    Just rd -> do
                                                        currentTime <- getCurrentTime

                                                        let channel = requestChannel rd
                                                            adjustedUntil = do
                                                                n <- nextUntil
                                                                return $ min n currentTime

                                                            newFilter = filter'' { until = adjustedUntil }

                                                        atomically $ do
                                                            writeTVar currentBatchVar Set.empty
                                                            writeTVar nextUntilVar Nothing

                                                        -- Resubscribe with new until
                                                        atomically $ writeTChan channel (NT.Close subId')
                                                        threadDelay 20000 -- 20ms
                                                        modify @RelayPool $ \st'' ->
                                                            st'' { subscriptions = Map.insert subId' (subInfo { subscriptionFilter = newFilter }) (subscriptions st') }

                                                        atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' newFilter)
                                            else do
                                                st' <- get @RelayPool
                                                case Map.lookup relayUri' (activeConnections st') of
                                                    Nothing -> pure ()
                                                    Just rd -> do
                                                        let channel = requestChannel rd
                                                        atomically $ writeTChan channel (NT.Close subId')
                                                        modify @RelayPool $ \st'' ->
                                                            st'' { subscriptions = Map.delete subId' (subscriptions st') }

                            SubscriptionClosed _ -> do
                                atomically $ writeTVar shouldStopVar True
                                modify @RelayPool $ \st' ->
                                    st' { subscriptions = Map.delete subId' (subscriptions st') }

                        shouldStop <- atomically $ readTVar shouldStopVar
                        unless shouldStop loop
                loop


processQueue :: (SubscriptionHandlerEff es, State RelayPool :> es)
            => Bool
            -> SubscriptionId
            -> TQueue (RelayURI, SubscriptionEvent)
            -> Eff es ()
processQueue stopOnEOSE subId' queue' = do
    shouldStopVar <- newTVarIO False
    let loop = do
            e <- atomically $ readTQueue queue'
            es <- atomically $ flushTQueue queue'

            forM_ (e:es) $ \(relayUri, e') -> do
                case e' of
                    EventAppeared event' -> handleEvent (Just relayUri) event'

                    SubscriptionEose _ -> do
                        when stopOnEOSE $ do
                            stopSubscription subId'
                            atomically $ writeTVar shouldStopVar True

                    SubscriptionClosed _ -> do
                        atomically $ writeTVar shouldStopVar True

            shouldStop <- atomically $ readTVar shouldStopVar
            unless shouldStop loop
    loop
