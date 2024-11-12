{-# LANGUAGE BlockArguments #-}

module Nostr.RelayConnection where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, forkIO)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM ( TChan, TMVar, atomically, newTChanIO, newTQueueIO
                                , newEmptyTMVarIO, newTMVarIO, putTMVar, readTChan, readTMVar
                                , tryPutTMVar, tryReadTMVar, takeTMVar, writeTChan, writeTQueue )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import Network.WebSockets qualified as WS
import Wuss qualified as Wuss

import EffectfulQML
import Logging
import Nostr
import Nostr.Event (createCanonicalAuthentication)
import Nostr.Keys (keyPairToPubKeyXO)
--import Nostr.Subscription
import Nostr.Types ( Event(..), RelayURI
                   , Request(..), Response(..), SubscriptionId )
import Nostr.Types qualified as NT
import Nostr.Util
import Types ( AppState(..), ConnectionError(..), ConnectionState(..)
             , PublishStatus(..),RelayPoolState(..), RelayData(..)
             , SubscriptionDetails(..), SubscriptionEvent(..), UIUpdates(..), emptyUpdates )


-- | Reason for disconnecting from a relay.
data DisconnectReason = UserInitiated | ConnectionFailure
    deriving (Show, Eq)


-- | Effect for handling RelayPool operations.
data RelayConnection :: Effect where
    ConnectRelay :: RelayURI -> RelayConnection m Bool
    DisconnectRelay :: RelayURI -> RelayConnection m ()


type instance DispatchOf RelayConnection = Dynamic


makeEffect ''RelayConnection


-- | RelayConnectionEff
type RelayConnectionEff es =
  ( State AppState :> es
  , State RelayPoolState :> es
  , Nostr :> es
  , EffectfulQML :> es
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
    ConnectRelay r -> do
        conns <- gets @RelayPoolState activeConnections
        if Map.member r conns
            then do
                let connState = connectionState <$> Map.lookup r conns
                case connState of
                    Just Connected -> do
                        logDebug $ "Already connected to " <> r
                        return True
                    Just Connecting -> do
                        logDebug $ "Connection already in progress for " <> r
                        return False
                    Just Disconnected -> do
                        -- Try to reconnect
                        chan <- newTChanIO
                        connectWithRetry r 5 chan
                    Nothing -> do
                        logWarning $ "No connection state found for relay: " <> r
                        return False
            else do
                chan <- newTChanIO
                let rd = RelayData
                            { connectionState = Connecting
                            , requestChannel = chan
                            , activeSubscriptions = Map.empty
                            , lastError = Nothing
                            , connectionAttempts = 0
                            , notices = []
                            , authenticated = False
                            }
                modify @RelayPoolState $ \st ->
                    st { activeConnections = Map.insert r rd (activeConnections st) }
                connectWithRetry r 5 chan

    DisconnectRelay r -> do
        st <- get @RelayPoolState
        case Map.lookup r (activeConnections st) of
            Just rd -> do
                void $ atomically $ writeTChan (requestChannel rd) NT.Disconnect
                modify @RelayPoolState $ \st' ->
                    st' { activeConnections = Map.delete r (activeConnections st') }
            Nothing -> return ()


-- | Connect with retry.
connectWithRetry :: RelayConnectionEff es => RelayURI -> Int -> TChan Request -> Eff es Bool
connectWithRetry r maxRetries requestChan = do
    st <- get @RelayPoolState
    
    let attempts = maybe 0 connectionAttempts $ Map.lookup r (activeConnections st)
    if attempts >= maxRetries
        then do
            modify @RelayPoolState $ \st' ->
                st' { activeConnections = Map.adjust 
                    (\d -> d { connectionState = Disconnected
                            , lastError = Just MaxRetriesReached 
                            }) 
                    r 
                    (activeConnections st') 
                }
            return False
        else do
            connectionMVar <- newEmptyTMVarIO

            let connectAction = if "wss://" `T.isPrefixOf` r
                    then Wuss.runSecureClient (T.unpack $ T.drop 6 r) 443 "/"
                    else WS.runClient (T.unpack $ T.drop 5 r) 80 "/"

            void $ forkIO $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
                let runClient = nostrClient connectionMVar r requestChan (maxRetries - attempts - 1) runE
                result <- try @SomeException $ connectAction runClient
                case result of
                    Right _ -> return ()
                    Left e -> runE $ do
                        atomically $ putTMVar connectionMVar False
                        logError $ "Connection error: " <> T.pack (show e)
                        st' <- get @RelayPoolState
                        when (Map.member r (activeConnections st')) $
                            modify @RelayPoolState $ \s ->
                                s { activeConnections = Map.adjust
                                    (\d -> d { connectionState = Disconnected
                                            , lastError = Just $ ConnectionFailed $ T.pack (show e)
                                            })
                                    r
                                    (activeConnections s)
                                }

            result <- atomically $ takeTMVar connectionMVar
            return result


-- | Nostr client for relay connections.
nostrClient :: RelayConnectionEff es => TMVar Bool -> RelayURI -> TChan Request -> Int -> (forall a. Eff es a -> IO a) -> WS.ClientApp ()
nostrClient connectionMVar r requestChan remainingRetries runE conn = runE $ do
    logDebug $ "Connected to " <> r
    void $ atomically $ putTMVar connectionMVar True
    modify @RelayPoolState $ \st ->
        st { activeConnections = Map.adjust (\d -> d { connectionState = Connected }) r (activeConnections st) }
    notifyRelayStatus
    updateQueue <- newTQueueIO
    terminateThreads <- newTMVarIO Nothing

    -- Websocket reading thread
    void $ forkIO $ let
        readLoop = do
            shouldTerminate <- atomically $ tryReadTMVar terminateThreads
            unless (isJust shouldTerminate) $ do
                msg <- liftIO (try (WS.receiveData conn) :: IO (Either SomeException BSL.ByteString))
                case msg of
                    Left ex -> do
                        logError $ "Error receiving data from " <> r <> ": " <> T.pack (show ex)
                        void $ atomically $ tryPutTMVar terminateThreads (Just ConnectionFailure)
                    Right msg' -> case eitherDecode msg' of
                        Right response -> do
                            updates <- handleResponse r response
                            atomically $ writeTQueue updateQueue updates
                            readLoop
                        Left err -> do
                            logError $ "Could not decode server response from " <> r <> ": " <> T.pack err
                            readLoop
        in readLoop

    -- Main message handling loop
    let loop = do
            msg <- atomically $ readTChan requestChan
            case msg of
                NT.Disconnect -> do
                    liftIO $ WS.sendClose conn (T.pack "Bye!")
                    void $ atomically $ tryPutTMVar terminateThreads (Just UserInitiated)
                _ -> do
                    result <- liftIO $ try @SomeException $ WS.sendTextData conn $ encode msg
                    case result of
                        Left ex -> do
                            logError $ "Error sending data to " <> r <> ": " <> T.pack (show ex)
                            void $ atomically $ tryPutTMVar terminateThreads (Just ConnectionFailure)
                        Right _ -> loop
    loop

    -- Handle cleanup and potential reconnection
    reason <- atomically $ fromMaybe ConnectionFailure <$> readTMVar terminateThreads
    modify @RelayPoolState $ \st ->
        st { activeConnections = Map.adjust (\d -> d { connectionState = Disconnected }) r (activeConnections st) }
    notifyRelayStatus

    unless (reason == UserInitiated) $ do
        logDebug $ "Reconnecting to: " <> r
        -- Only attempt reconnection for non-user-initiated disconnects
        void $ connectWithRetry r remainingRetries requestChan
        -- Resubscribe active subscriptions
        st <- get @RelayPoolState
        let relaySubs = case Map.lookup r (activeConnections st) of
                Just rd -> Map.elems (activeSubscriptions rd)
                Nothing -> []
        forM_ relaySubs $ \sub -> do
            let sub' = NT.Subscription (subscriptionId sub) (subscriptionFilters sub)
            atomically $ writeTChan requestChan (NT.Subscribe sub')


-- | Handle responses.
handleResponse :: RelayConnectionEff es => RelayURI -> Response -> Eff es UIUpdates
handleResponse relayURI' r = case r of
    EventReceived subId' event' -> do
        recordLatestCreatedAt relayURI' event'
        enqueueEvent subId' (EventAppeared event') -- @todo check against filters?
        return emptyUpdates
        where
            recordLatestCreatedAt :: RelayConnectionEff es => RelayURI -> Event -> Eff es ()
            recordLatestCreatedAt r' e = do
                modify @RelayPoolState $ \st -> st { activeConnections = Map.adjust
                    (\rd -> rd { activeSubscriptions = Map.adjust
                        (\subDetails -> if createdAt e > newestCreatedAt subDetails
                                        then subDetails { newestCreatedAt = createdAt e }
                                        else subDetails)
                        subId'
                        (activeSubscriptions rd)
                    })
                    r'
                    (activeConnections st)
                }

    Eose subId' -> do
        enqueueEvent subId' SubscriptionEose
        return emptyUpdates

    Closed subId' msg -> do
        enqueueEvent subId' (SubscriptionClosed msg)
        modify @RelayPoolState $ \st ->
            st { activeConnections = Map.adjust
                (\rd -> rd { activeSubscriptions = Map.delete subId' (activeSubscriptions rd) })
                relayURI'
                (activeConnections st)
            }
        return emptyUpdates

    Ok eventId' accepted' msg -> do
        modify @RelayPoolState $ \st ->
            st { publishStatus = Map.adjust (\relayMap -> 
                    Map.insert relayURI' (if accepted' then Success else Failure msg) relayMap
                ) eventId' (publishStatus st) }
        return $ emptyUpdates { publishStatusChanged = True }
        
    Notice msg -> do
        modify @RelayPoolState $ \st ->
            st { activeConnections = Map.adjust
                (\rd -> rd { notices = msg : notices rd })
                relayURI'
                (activeConnections st)
            }
        return $ emptyUpdates { noticesChanged = True }

    Auth challenge -> do
        now <- getCurrentTime
        kp <- getKeyPair
        let unsignedEvent = createCanonicalAuthentication relayURI' challenge (keyPairToPubKeyXO kp) now
        signedEventMaybe <- signEvent unsignedEvent kp
        case signedEventMaybe of
            Just signedEvent -> do
                st <- get @RelayPoolState
                case Map.lookup relayURI' (activeConnections st) of
                    Just rd -> do
                        atomically $ writeTChan (requestChannel rd) (Authenticate signedEvent)
                        modify @RelayPoolState $ \st' ->
                            st' { activeConnections = Map.adjust (\rd' -> rd' { authenticated = True }) relayURI' (activeConnections st') }
                        -- @todo re-sending events?
                    Nothing -> logError $ "Error handling relay authentication, no channel found: " <> relayURI'
            Nothing -> logError "Failed to sign canonical authentication event"
        return emptyUpdates
    where
        enqueueEvent :: RelayConnectionEff es => SubscriptionId -> SubscriptionEvent -> Eff es ()
        enqueueEvent subId' event' = do
            st <- get @RelayPoolState
            case Map.lookup relayURI' (activeConnections st) of
                Just rd -> case Map.lookup subId' (activeSubscriptions rd) of
                    Just sd -> atomically $ writeTQueue (responseQueue sd) event'
                    Nothing -> error $ "No subscription found for " <> show subId'
                Nothing -> error $ "No connection found for " <> show relayURI'
