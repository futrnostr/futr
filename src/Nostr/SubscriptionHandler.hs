module Nostr.SubscriptionHandler where

import Control.Monad (forM_,unless, when)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TQueue, TVar, atomically, flushTQueue, newTVarIO, readTQueue, readTVar, writeTChan, writeTQueue, writeTVar )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import Prelude hiding (until)

-- DEBUG IMPORTS
import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Crypto.Hash.SHA256 qualified as SHA256
import Nostr.Event (validateEventId, verifySignature)
-- END DEBUG IMPORTS

import QtQuick
import KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (validateEvent)
import Nostr.Keys (byteStringToHex, keyPairToPubKeyXO)
import Nostr.Subscription
import Nostr.Types (Event(..), EventId(..), Filter(..), Kind(..), RelayURI, SubscriptionId)
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
    HandleSubscription subId' queue -> processQueue False subId' queue

    HandleSubscriptionUntilEOSE subId' queue -> processQueue True subId' queue

    HandlePaginationSubscription subId' queue -> do
        shouldStopVar <- newTVarIO False
        let loop = do
                e  <- atomically $ readTQueue queue
                es <- atomically $ flushTQueue queue
                forM_ (e:es) $ \(relayUri, e') -> do
                    case e' of
                        EventAppeared event' -> do
                            handleEvent relayUri event'
                        SubscriptionEose _ -> do
                            subs <- gets @RelayPool subscriptions
                            let subInfo = Map.findWithDefault
                                            (error "Subscription not found")
                                            subId'
                                            subs
                                filter = subscriptionFilter subInfo
                                relayUri = relay subInfo
                                newUntil = (oldestCreatedAt subInfo) - 1
                                shouldPaginate = maybe False (\l -> eventsProcessed subInfo >= l && l /= 0) (limit filter)
                                                && maybe True (\s -> newUntil > s) (since filter)
                            
                            if shouldPaginate
                                then do
                                    st <- get @RelayPool
                                    case Map.lookup relayUri (activeConnections st) of
                                        Just rd -> do
                                            let channel = requestChannel rd
                                                newFilter = filter { until = Just newUntil }
                                                newSub = subInfo { subscriptionFilter = newFilter }

                                            atomically $ writeTChan channel (NT.Close subId')
                                            modify @RelayPool $ \st' ->
                                                st' { subscriptions = Map.insert subId' newSub (subscriptions st') }
                                            atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' newFilter)

                                        Nothing -> pure ()
                                else pure ()

                        SubscriptionClosed _ -> do
                            stoppingSubs <- gets @RelayPool stoppingSubscriptions
                            when (subId' `elem` stoppingSubs) $ do
                                atomically $ writeTVar shouldStopVar True
                                modify @RelayPool $ \st ->
                                    st { stoppingSubscriptions = filter (/= subId') (stoppingSubscriptions st) }

                shouldStop <- atomically $ readTVar shouldStopVar
                unless shouldStop loop
        loop


processQueue :: forall es m. (SubscriptionHandlerEff es, State RelayPool :> es) 
            => Bool 
            -> SubscriptionId 
            -> TQueue (RelayURI, SubscriptionEvent) 
            -> Eff es ()
processQueue stopOnEOSE subId' queue = do
    shouldStopVar <- newTVarIO False
    let loop = do
            e <- atomically $ readTQueue queue
            es <- atomically $ flushTQueue queue
        
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

            let unsignedEvent = NT.UnsignedEvent (pubKey event') (createdAt event') (kind event') (tags event') (content event')
                serializedEvent = BS.toStrict $ encode unsignedEvent
                computedId = SHA256.hash serializedEvent
                eventId' = getEventId $ eventId event'

            logWarning $ "Raw event: " <> pack (show serializedEvent)
            logWarning $ "Event: " <> pack (show event')
            
            pure emptyUpdates
        else do
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
