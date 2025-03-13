module Nostr.SubscriptionHandler where

import Control.Monad (forM_, unless, when)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue, newTVarIO, readTQueue, readTVar, writeTChan, writeTQueue, writeTVar, modifyTVar)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import Prelude hiding (until)

-- DEBUG IMPORTS
import Data.Aeson (encode)
import Data.ByteString qualified as BS
--import Crypto.Hash.SHA256 qualified as SHA256
import Nostr.Event (validateEventId, verifySignature)
-- END DEBUG IMPORTS

import QtQuick
import KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (Event(..), EventId(..), Kind(..), UnsignedEvent(..), validateEvent)
import Nostr.Keys (byteStringToHex, keyPairToPubKeyXO)
import Nostr.Relay (RelayURI)
import Nostr.Subscription
import Nostr.Types (Filter(..), SubscriptionId)
import Nostr.Types qualified as NT
import Nostr.Util
import Store.Lmdb
import Types

-- | Subscription effects
data SubscriptionHandler :: Effect where
    HandleSubscription :: SubscriptionId -> TQueue (RelayURI, SubscriptionEvent) -> SubscriptionHandler m ()
    HandleSubscriptionUntilEOSE :: SubscriptionId -> TQueue (RelayURI, SubscriptionEvent) -> SubscriptionHandler m ()
    HandlePaginationSubscription :: SubscriptionId -> TQueue (RelayURI, SubscriptionEvent) -> SubscriptionHandler m ()

type instance DispatchOf SubscriptionHandler = Dynamic

makeEffect ''SubscriptionHandler

-- | SubscriptionEff
type SubscriptionHandlerEff es =
  ( Subscription :> es
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
    HandleSubscription subId' queue' -> processQueue False subId' queue'

    HandleSubscriptionUntilEOSE subId' queue' -> processQueue True subId' queue'

    HandlePaginationSubscription subId' queue' -> do
        shouldStopVar <- newTVarIO False
        seenEventsVar <- newTVarIO Set.empty
        currentBatchVar <- newTVarIO Set.empty
        nextUntilVar <- newTVarIO Nothing
        let loop = do
                e  <- atomically $ readTQueue queue'
                es <- atomically $ flushTQueue queue'
                let events = e:es

                seenEvents <- atomically $ readTVar seenEventsVar
                let newEvents = filter (\(_, e') ->
                        case e' of
                            EventAppeared evt -> not $ Set.member (getEventId $ eventId evt) seenEvents
                            _ -> True
                        ) events

                forM_ newEvents $ \(relayUri, e') -> case e' of
                    EventAppeared event' -> do
                        handleEvent relayUri event'
                        atomically $ do
                            modifyTVar seenEventsVar $ Set.insert (getEventId $ eventId event')
                            modifyTVar currentBatchVar $ Set.insert (getEventId $ eventId event')
                            modifyTVar nextUntilVar $ \current -> case current of
                                Nothing -> Just (createdAt event')
                                Just t -> Just (min t (createdAt event'))

                    SubscriptionEose _ -> do
                        subs <- gets @RelayPool subscriptions
                        case Map.lookup subId' subs of
                            Nothing ->
                                logWarning $ "Pagination subscription " <> pack (show subId') <> " not found"
                            Just subInfo -> do
                                let filter' = subscriptionFilter subInfo
                                    relayUri' = relay subInfo
                                    requestedLimit = limit filter'
                                    seenEventsCount = Set.size seenEvents

                                currentBatch <- atomically $ readTVar currentBatchVar
                                nextUntil <- atomically $ readTVar nextUntilVar

                                -- Only continue if we got new events in this batch
                                let shouldPaginate = not (Set.null currentBatch)
                                        && maybe True (\l -> seenEventsCount < l) requestedLimit
                                        && isJust nextUntil
{-
                                logDebug $ "Pagination check for " <> relayUri' <> " (sub " <> pack (show subId') <> ") - "
                                        <> "Should paginate: " <> pack (show shouldPaginate)
                                        <> ", Events in batch: " <> pack (show (Set.size currentBatch))
                                        <> ", Total events: " <> pack (show seenEventsCount)
                                        <> ", Requested limit: " <> pack (show requestedLimit)
                                        <> ", Next until: " <> pack (show nextUntil)
-}
                                when shouldPaginate $ do
                                    st <- get @RelayPool
                                    case Map.lookup relayUri' (activeConnections st) of
                                        Nothing -> pure ()

                                        Just rd -> do
                                            currentTime <- getCurrentTime

                                            let channel = requestChannel rd
                                                adjustedUntil = do
                                                    n <- nextUntil
                                                    return $ min n currentTime

                                                newFilter = filter' { until = adjustedUntil }
{-
                                            logDebug $ "Paginating subscription " <> pack (show subId')
                                                    <> " with new until: " <> pack (show adjustedUntil)
-}
                                            -- Clear current batch cache and nextUntil
                                            atomically $ do
                                                writeTVar currentBatchVar Set.empty
                                                writeTVar nextUntilVar Nothing

                                            -- Resubscribe with new until
                                            atomically $ writeTChan channel (NT.Close subId')
                                            modify @RelayPool $ \st' ->
                                                st' { subscriptions = Map.insert subId' (subInfo { subscriptionFilter = newFilter }) (subscriptions st') }
                                            atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' newFilter)

                    SubscriptionClosed _ -> do
                        stoppingSubs <- gets @RelayPool stoppingSubscriptions
                        when (subId' `elem` stoppingSubs) $ do
                            atomically $ writeTVar shouldStopVar True
                            modify @RelayPool $ \st ->
                                st { stoppingSubscriptions = filter (/= subId') (stoppingSubscriptions st) }

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
                    EventAppeared event' -> handleEvent relayUri event'

                    SubscriptionEose _ -> when stopOnEOSE $ stopSubscription subId'

                    SubscriptionClosed _ -> atomically $ writeTVar shouldStopVar True

            shouldStop <- atomically $ readTVar shouldStopVar
            unless shouldStop loop

    loop


handleEvent :: SubscriptionHandlerEff es => RelayURI -> Event -> Eff es ()
handleEvent r event' = do
    updates <- if not (validateEvent event')
        then do
            logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))

            logWarning $ "EventID: " <> if validateEventId event' then "valid" else "invalid"
            logWarning $ "Signature: " <> if verifySignature event' then "valid" else "invalid"

            let unsignedEvent = UnsignedEvent (pubKey event') (createdAt event') (kind event') (tags event') (content event')
                serializedEvent = BS.toStrict $ encode unsignedEvent
                --computedId = SHA256.hash serializedEvent
                --eventId' = getEventId $ eventId event'

            logWarning $ "Raw event: " <> pack (show serializedEvent)
            logWarning $ "Event: " <> pack (show event')

            pure emptyUpdates
        else do
            --logDebug $ "Seen event: " <> pack (show $ kind event') <> " " <> pack (show $ eventId event') <> " on relay: " <> r
            let ev = EventWithRelays event' (Set.singleton r)
            wasUpdated <- putEvent ev
            case kind event' of
                ShortTextNote -> do
                    pure $ emptyUpdates { postsChanged = wasUpdated }

                Repost -> do
                    pure $ emptyUpdates { postsChanged = wasUpdated }

                EventDeletion ->
                    pure $ emptyUpdates { postsChanged = wasUpdated, privateMessagesChanged = wasUpdated }

                Metadata -> do
                    case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                        Right profile -> do
                            kp <- getKeyPair
                            let isOwnProfile = pubKey event' == keyPairToPubKeyXO kp
                            when isOwnProfile $ do
                                let aid = AccountId $ pubKeyXOToBech32 (pubKey event')
                                updateProfile aid profile
                            pure $ emptyUpdates { profilesChanged = wasUpdated, myFollowsChanged = wasUpdated }
                        Left err -> do
                            logWarning $ "Failed to decode metadata: " <> pack err
                            pure emptyUpdates

                FollowList -> do
                    kp <- getKeyPair
                    let pk = keyPairToPubKeyXO kp
                    pure $ emptyUpdates { followsChanged = wasUpdated, myFollowsChanged = wasUpdated && pk == pubKey event' }

                GiftWrap -> do
                    pure $ emptyUpdates { privateMessagesChanged = wasUpdated }

                RelayListMetadata -> do
                    pure $ emptyUpdates { generalRelaysChanged = wasUpdated }

                PreferredDMRelays -> do
                    pure $ emptyUpdates { dmRelaysChanged = wasUpdated }

                _ -> do
                    logDebug $ "Ignoring event of kind: " <> pack (show (kind event'))
                    pure emptyUpdates

    when (myFollowsChanged updates || dmRelaysChanged updates || generalRelaysChanged updates) $ do
        -- notify the inbox model to update the subscriptions
        q <- gets @RelayPool updateQueue
        atomically $ writeTQueue q ()

    notify updates
