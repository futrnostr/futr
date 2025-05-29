{-# LANGUAGE BlockArguments #-}

module Nostr.RelayConnection where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (dropWhileEnd, find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, forkIO, threadDelay)
import Effectful.Concurrent.Async (async, waitAnyCancel)
import Effectful.Concurrent.STM ( TChan, TMVar, atomically, newTChanIO
                                , newEmptyTMVarIO, putTMVar, readTChan
                                , takeTMVar, writeTChan, writeTQueue )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Network.URI (URI(..), parseURI, uriAuthority, uriPort, uriRegName, uriScheme)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection.PingPong (defaultPingPongOptions, withPingPong)
import Wuss qualified as Wuss

import QtQuick
import Logging
import Nostr
import Nostr.Event (Event(..), createCanonicalAuthentication)
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Relay (RelayURI)
import Nostr.Types (Response(..), SubscriptionId)
import Nostr.Types qualified as NT
import Nostr.Util
import Types ( AppState(..), ConnectionError(..), ConnectionState(..)
             , PublishStatus(..), RelayPool(..), RelayData(..)
             , SubscriptionState(..), SubscriptionEvent(..))


-- | Reason for disconnecting from a relay.
data DisconnectReason = UserInitiated | ConnectionFailure
    deriving (Show, Eq)


-- | Effect for handling RelayPool operations.
data RelayConnection :: Effect where
    Connect :: RelayURI -> RelayConnection m Bool
    Disconnect :: RelayURI -> RelayConnection m ()


type instance DispatchOf RelayConnection = Dynamic


connect :: RelayConnection :> es => RelayURI -> Eff es Bool
connect uri = send $ Connect uri

disconnect :: RelayConnection :> es => RelayURI -> Eff es ()
disconnect uri = send $ Disconnect uri


-- | RelayConnectionEff
type RelayConnectionEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , Nostr :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  , Util :> es
  , IOE :> es
  )


-- | Handler for relay pool effects.
runRelayConnection
  :: RelayConnectionEff es
  => Eff (RelayConnection : es) a
  -> Eff es a
runRelayConnection = interpret $ \_ -> \case
    Connect r -> do
        let r' = normalizeRelayURI r
        conns <- gets @RelayPool activeConnections
        if Map.member r' conns
            then do
                let connState = connectionState <$> Map.lookup r' conns
                case connState of
                    Just Connected -> return True
                    Just Connecting -> return False
                    Just Disconnected -> do
                        -- Try to reconnect
                        chan <- newTChanIO
                        connectWithRetry r' 5 chan
                    Nothing -> do
                        return False
            else do
                chan <- newTChanIO
                let rd = RelayData
                            { connectionState = Connecting
                            , requestChannel = chan
                            , lastError = Nothing
                            , connectionAttempts = 0
                            , notices = []
                            , pendingRequests = []
                            , pendingEvents = []
                            , pendingAuthId = Nothing
                            }
                modify @RelayPool $ \st ->
                    st { activeConnections = Map.insert r' rd (activeConnections st) }
                connectWithRetry r' 5 chan

    Disconnect r -> do
        let r' = normalizeRelayURI r
        st <- get @RelayPool
        case Map.lookup r' (activeConnections st) of
            Just rd -> do
                void $ atomically $ writeTChan (requestChannel rd) NT.Disconnect
                modify @RelayPool $ \st' ->
                    st' { activeConnections = Map.adjust
                        (\d -> d { connectionState = Disconnected })
                        r'
                        (activeConnections st')
                    }
            Nothing -> return ()


-- | Connect with retry.
connectWithRetry :: RelayConnectionEff es => RelayURI -> Int -> TChan NT.Request -> Eff es Bool
connectWithRetry r maxRetries requestChan = do
    st <- get @RelayPool

    let attempts = maybe 0 connectionAttempts $ Map.lookup r (activeConnections st)
    if attempts >= maxRetries
        then do
            modify @RelayPool $ \st' ->
                st' { activeConnections = Map.adjust
                    (\d -> d { connectionState = Disconnected
                            , lastError = Just MaxRetriesReached
                            })
                    r
                    (activeConnections st')
                }
            return False
        else do
            -- Add exponential backoff delay
            when (attempts > 0) $ do
                let delayMs = min 5000000 (1000000 * (2 ^ attempts))  -- Cap at 5 seconds
                threadDelay delayMs

            modify @RelayPool $ \st' ->
                st' { activeConnections = Map.adjust
                    (\d -> d { connectionState = Connecting
                            , connectionAttempts = connectionAttempts d + 1
                            })
                    r
                    (activeConnections st')
                }

            connectionMVar <- newEmptyTMVarIO

            let connectAction = case parseURI (T.unpack r) of
                    Just uri -> case uriAuthority uri of
                        Just auth ->
                            let host = uriRegName auth
                                port = case uriPort auth of
                                    "" -> if "wss://" `T.isPrefixOf` r then 443 else 80
                                    p -> read (drop 1 p) -- drop the leading ':'
                            in if "wss://" `T.isPrefixOf` r
                                then Wuss.runSecureClient host port "/"
                                else WS.runClient host (fromIntegral port) "/"
                        Nothing -> error $ "Invalid relay URI (no authority): " ++ T.unpack r
                    Nothing -> error $ "Invalid relay URI: " ++ T.unpack r

            void $ forkIO $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
                let runClient = nostrClient connectionMVar r requestChan runE
                result <- try @SomeException $ connectAction runClient
                case result of
                    Right _ -> return ()
                    Left e -> runE $ do
                        atomically $ putTMVar connectionMVar False
                        st' <- get @RelayPool
                        when (Map.member r (activeConnections st')) $
                            modify @RelayPool $ \s ->
                                s { activeConnections = Map.adjust
                                    (\d -> d { connectionState = Disconnected
                                            , lastError = Just $ ConnectionFailed $ T.pack (show e)
                                            })
                                    r
                                    (activeConnections s)
                                }

            atomically $ takeTMVar connectionMVar


-- | Nostr client for relay connections.
nostrClient :: RelayConnectionEff es => TMVar Bool -> RelayURI -> TChan NT.Request -> (forall a. Eff es a -> IO a) -> WS.ClientApp ()
nostrClient connectionMVar r requestChan runE conn = runE $ do
    liftIO $ withPingPong defaultPingPongOptions conn $ \conn' -> runE $ do
        modify @RelayPool $ \st ->
            st { activeConnections = Map.adjust
                (\d -> d { connectionState = Connected
                        , requestChannel = requestChan
                        })
                r
                (activeConnections st)
            }
        notifyRelayStatus

        -- Handle pending subscriptions
        st <- get @RelayPool
        let pendingSubs = pendingSubscriptions st
        forM_ (Map.toList pendingSubs) $ \(subId', details) -> do
            atomically $ writeTChan requestChan (NT.Subscribe $ NT.Subscription subId' (subscriptionFilter details))

        -- Move pending subscriptions to active subscriptions
        modify @RelayPool $ \st' ->
            st' { subscriptions = Map.union (subscriptions st') pendingSubs
                , pendingSubscriptions = Map.empty
                }

        void $ atomically $ putTMVar connectionMVar True

        receiveThread <- async $ receiveLoop conn'
        sendThread <- async $ sendLoop conn'
        void $ waitAnyCancel [receiveThread, sendThread]
        modify @RelayPool $ \st' ->
            st' { activeConnections = Map.adjust (\d -> d { connectionState = Disconnected }) r (activeConnections st') }
        notifyRelayStatus

  where
    receiveLoop conn' = do
        msg <- liftIO (try (WS.receiveData conn') :: IO (Either SomeException BSL.ByteString))
        case msg of
            Left _ -> return ()  -- Exit the loop on error
            Right msg' -> case eitherDecode msg' of
                Right response -> do
                    updates <- handleResponse r response
                    notify updates
                    receiveLoop conn'
                Left err -> do
                    logError $ "Could not decode server response from " <> r <> ": " <> T.pack err
                    logError $ "Msg: " <> T.pack (show msg')
                    receiveLoop conn'

    sendLoop conn' = do
        msg <- atomically $ readTChan requestChan
        case msg of
            NT.Disconnect -> do
                liftIO $ WS.sendClose conn' (T.pack "Bye!")
                return ()
            NT.SendEvent event -> do
                result <- liftIO $ try @SomeException $ WS.sendTextData conn' $ encode msg
                case result of
                    Left ex -> do
                        logError $ "Error sending data to " <> r <> ": " <> T.pack (show ex)
                        return ()
                    Right _ -> do
                        -- Store the event in the state for potential retry
                        modify @RelayPool $ \st ->
                            st { activeConnections = Map.adjust
                                (\rd -> rd { pendingEvents = event : pendingEvents rd })
                                r
                                (activeConnections st)
                            }
                        sendLoop conn'
            _ -> do
                result <- liftIO $ try @SomeException $ WS.sendTextData conn' $ encode msg
                case result of
                    Left ex -> do
                        logError $ "Error sending data to " <> r <> ": " <> T.pack (show ex)
                        return ()
                    Right _ -> sendLoop conn'


-- | Handle responses.
handleResponse :: RelayConnectionEff es => RelayURI -> Response -> Eff es UIUpdates
handleResponse relayURI' r = case r of
    EventReceived subId' event' -> do
        recordOldestCreatedAt subId' event'
        modify @RelayPool $ \st -> st
            { subscriptions = Map.adjust
                (\subDetails -> subDetails { eventsProcessed = eventsProcessed subDetails + 1 })
                subId'
                (subscriptions st)
            }
        enqueueEvent subId' (EventAppeared event') -- @todo check against filters?
        return emptyUpdates
        where
            recordOldestCreatedAt :: RelayConnectionEff es => SubscriptionId -> Event -> Eff es ()
            recordOldestCreatedAt sid e = do
                modify @RelayPool $ \st -> st
                    { subscriptions = Map.adjust
                        (\subDetails -> if createdAt e < oldestCreatedAt subDetails
                                      then subDetails { oldestCreatedAt = createdAt e }
                                      else subDetails)
                        sid
                        (subscriptions st)
                    }

    Eose subId' -> do
        enqueueEvent subId' (SubscriptionEose subId')
        return emptyUpdates

    Closed subId' msg -> do
        if "auth-required" `T.isPrefixOf` msg
            then do
                -- Queue the subscription for retry after authentication
                st <- get @RelayPool
                case Map.lookup subId' (subscriptions st) of
                    Just subDetails -> do
                        let subscription = NT.Subscription
                                { NT.subId = subId'
                                , NT.filter = subscriptionFilter subDetails
                                }
                        handleAuthRequired relayURI' (NT.Subscribe subscription)
                    Nothing -> logError $ "No subscription found for " <> T.pack (show subId')
            else do
                st <- get @RelayPool
                case Map.lookup subId' (subscriptions st) of
                    Just sd -> do
                        atomically $ writeTQueue (responseQueue sd) (relayURI', SubscriptionClosed msg)
                        modify @RelayPool $ \st' ->
                            st' { subscriptions = Map.delete subId' (subscriptions st') }
                    Nothing -> pure ()

        return emptyUpdates

    Ok eventId' accepted' msg -> do
        let isAuthRequired = maybe False ("auth-required" `T.isPrefixOf`) msg

        when (not isAuthRequired && isJust eventId') $ do
            modify @RelayPool $ \st -> st {
                publishStatus = Map.adjust
                    (Map.insert relayURI' $
                        if accepted'
                            then Success
                            else Failure (fromMaybe "Rejected by relay" msg))
                    (fromMaybe (error "No event ID") eventId')
                    (publishStatus st)
            }

        if isAuthRequired
            then do
                st <- get @RelayPool
                case Map.lookup relayURI' (activeConnections st) of
                    Just rd ->
                        case eventId' of
                            Just eid -> case find (\e -> eventId e == eid) (pendingEvents rd) of
                                Just event -> handleAuthRequired relayURI' (NT.SendEvent event)
                                Nothing -> logDebug $ "No pending event found for " <> T.pack (show eid)
                            Nothing -> logError "Received auth-required but no event ID"
                    Nothing -> logError $ "Received auth-required but no connection found: " <> relayURI'
            else do
                st <- get @RelayPool
                case Map.lookup relayURI' (activeConnections st) of
                    Just rd ->
                        case (pendingAuthId rd, eventId') of
                            (Just authId, Just eid) | authId == eid && accepted' -> do
                                let pendingReqs = pendingRequests rd
                                let pendingEvts = pendingEvents rd
                                {-
                                logDebug $ "Auth successful, retrying " <> T.pack (show (length pendingReqs))
                                        <> " pending requests and "
                                        <> T.pack (show (length pendingEvts))
                                        <> " pending events for " <> relayURI'
                                -}

                                -- Clear pending lists and auth ID
                                modify @RelayPool $ \st' ->
                                    st' { activeConnections = Map.adjust
                                        (\rd' -> rd' { pendingRequests = []
                                                   , pendingEvents = []
                                                   , pendingAuthId = Nothing
                                                   })
                                        relayURI'
                                        (activeConnections st')
                                    }

                                -- Retry events and requests
                                forM_ pendingEvts $ \evt -> atomically $ writeTChan (requestChannel rd) (NT.SendEvent evt)
                                forM_ pendingReqs $ \req -> atomically $ writeTChan (requestChannel rd) req

                            _ -> pure ()
                    Nothing -> logError $ "Received OK but no connection found: " <> relayURI'

        return $ emptyUpdates { publishStatusChanged = True }

    Notice msg -> do
        modify @RelayPool $ \st ->
            st { activeConnections = Map.adjust
                (\rd -> rd { notices = msg : notices rd })
                relayURI'
                (activeConnections st)
            }
        return $ emptyUpdates { noticesChanged = True }

    Auth challenge -> do
        st <- get @RelayPool
        case Map.lookup relayURI' (activeConnections st) of
            Just rd -> do
                now <- getCurrentTime
                kp <- getKeyPair
                let unsignedEvent = createCanonicalAuthentication relayURI' challenge (keyPairToPubKeyXO kp) now
                signedEventMaybe <- signEvent unsignedEvent kp
                case signedEventMaybe of
                    Just signedEvent -> do
                        modify @RelayPool $ \st' ->
                            st' { activeConnections = Map.adjust
                                (\rd' -> rd' { pendingAuthId = Just (eventId signedEvent) })
                                relayURI'
                                (activeConnections st')
                            }
                        atomically $ writeTChan (requestChannel rd) (NT.Authenticate signedEvent)
                        return emptyUpdates
                    Nothing -> do
                        logError "Failed to sign canonical authentication event"
                        return emptyUpdates
            Nothing -> do
                logError $ "Error handling relay authentication, no channel found: " <> relayURI'
                return emptyUpdates

    where
        enqueueEvent :: RelayConnectionEff es => SubscriptionId -> SubscriptionEvent -> Eff es ()
        enqueueEvent subId' event' = do
            st <- get @RelayPool
            case Map.lookup subId' (subscriptions st) of
                Just sd -> atomically $ writeTQueue (responseQueue sd) (relayURI', event')
                Nothing -> pure () --logError $ "No response queue for subscription found: " <> T.pack (show subId')


-- | Handle authentication required.
handleAuthRequired :: RelayConnectionEff es => RelayURI -> NT.Request -> Eff es ()
handleAuthRequired relayURI' request = case request of
    NT.SendEvent evt -> do
        modify @RelayPool $ \st' ->
            st' { activeConnections = Map.adjust
                (\rd' -> rd' { pendingEvents = evt : pendingEvents rd' })
                relayURI'
                (activeConnections st')
            }
    _ -> do
        modify @RelayPool $ \st' ->
            st' { activeConnections = Map.adjust
                (\rd' -> rd' { pendingRequests = request : pendingRequests rd' })
                relayURI'
                (activeConnections st')
            }
-- | Normalize a relay URI according to RFC 3986
normalizeRelayURI :: RelayURI -> RelayURI
normalizeRelayURI uri = case parseURI (T.unpack uri) of
    Just uri' -> T.pack $
        (if uriScheme uri' == "wss:" then "wss://" else "ws://") ++
        maybe "" (\auth ->
            let hostPort = uriRegName auth ++
                    case uriPort auth of
                        ":80" | uriScheme uri' == "ws:" -> ""
                        ":443" | uriScheme uri' == "wss:" -> ""
                        p -> p
            in hostPort
        ) (uriAuthority uri') ++
        dropWhileEnd (== '/') (uriPath uri' ++ uriQuery uri' ++ uriFragment uri')
    Nothing -> uri
