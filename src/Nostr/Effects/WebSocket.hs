{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Nostr.Effects.WebSocket where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, forkIO, threadDelay)
import Effectful.Concurrent.Async (async, race, waitAnyCancel)
import Effectful.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Effectful.Concurrent.STM (TChan, TQueue, atomically, readTChan, writeTQueue)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, modify)
import Effectful.TH
import Control.Exception qualified as E
import Network.WebSockets qualified as WS
import Wuss qualified as Wuss

import Nostr.Effects.Logging
import Nostr.Types


-- | WebSocket connection state.
data ConnectionState = Connected | Disconnected | Connecting
  deriving (Show, Eq)


-- | WebSocket connection state for a specific relay.
data RelayConnectionState = RelayConnectionState
  { connectionStatus :: ConnectionState
  , connectionRetries :: Int
  }


-- | WebSocket state for all relays.
data WebSocketState = WebSocketState
  { connections :: Map.Map RelayURI RelayConnectionState
  , maxRetries :: Int
  }


-- | Custom error type for WebSocket connections
data ConnectionError
  = ConnectionFailed
  | Timeout
  | InvalidRelayConfig
  | MaxRetriesReached
  deriving (Show, Eq)


-- | Effect for handling WebSocket operations.
data WebSocket :: Effect where
    RunClient :: Relay -> TChan Request -> TQueue Response -> WebSocket m (Either ConnectionError ())


-- | Dispatch for WebSocket.
type instance DispatchOf WebSocket = Dynamic


makeEffect ''WebSocket


-- | Effectful environment for WebSocket operations.
type WebSocketEff es = (Concurrent :> es, IOE :> es, Logging :> es)


-- | Handler for web socket effects.
runWebSocket
  :: WebSocketEff es
  => Int  -- ^ Maximum number of reconnection attempts
  -> Eff (WebSocket : State WebSocketState : es) a
  -> Eff es a
runWebSocket maxRetries' action = evalState (WebSocketState Map.empty maxRetries') $ interpret handleWebSocket action
  where
    handleWebSocket :: WebSocketEff es => EffectHandler WebSocket (State WebSocketState : es)
    handleWebSocket _ = \case
        RunClient relay requestChan responseQueue -> do
            result <- runClientWithRetry relay requestChan responseQueue
            case result of
                Left err -> do
                    logError $ "Failed to connect after max retries: " <> T.pack (show err)
                    return $ Left err
                Right _ -> return $ Right ()

runClientWithRetry
  :: (WebSocketEff es, State WebSocketState :> es)
  => Relay
  -> TChan Request
  -> TQueue Response
  -> Eff es (Either ConnectionError ())
runClientWithRetry relay requestChan responseQueue = go
  where
    go = do
      st <- get @WebSocketState
      let relayState = Map.findWithDefault (RelayConnectionState Disconnected 0) (uri relay) (connections st)
      if connectionRetries relayState >= maxRetries st
        then do
          logError $ "Max retries reached for " <> relayName relay
          return $ Left MaxRetriesReached
        else do
          modify @WebSocketState $ \s -> s { connections = Map.insert (uri relay) (relayState { connectionStatus = Connecting }) (connections s) }
          result <- attemptConnection relay requestChan responseQueue
          case result of
            Left err -> do
              logWarning $ "Connection attempt failed: " <> T.pack (show err)
              modify @WebSocketState $ \s -> 
                let updatedState = relayState { connectionRetries = connectionRetries relayState + 1, connectionStatus = Disconnected }
                in s { connections = Map.insert (uri relay) updatedState (connections s) }
              backoffTime <- calculateBackoffTime (connectionRetries relayState)
              logDebug $ "Retrying in " <> T.pack (show backoffTime) <> " seconds"
              threadDelay $ backoffTime * 1000000
              go
            Right _ -> do
              modify @WebSocketState $ \s -> 
                let updatedState = RelayConnectionState Connected 0
                in s { connections = Map.insert (uri relay) updatedState (connections s) }
              return $ Right ()

calculateBackoffTime :: Int -> Eff es Int
calculateBackoffTime retries = do
  let baseDelay = 1
      maxDelay = 60
  return $ min maxDelay (baseDelay * (2 ^ retries))

attemptConnection
  :: (WebSocketEff es, State WebSocketState :> es)
  => Relay
  -> TChan Request
  -> TQueue Response
  -> Eff es (Either ConnectionError ())
attemptConnection relay requestChan responseQueue = do
    case (extractHostname relay, extractScheme relay) of
        (Just host', Just scheme') -> do
            let options = WS.defaultConnectionOptions
                  { WS.connectionCompressionOptions =
                      WS.PermessageDeflateCompression WS.defaultPermessageDeflate
                  }
            let connectAction = case scheme' of
                  "wss" -> Wuss.runSecureClientWith
                      (T.unpack host')
                      (fromIntegral $ extractPort relay)
                      (T.unpack $ extractPath relay)
                      options
                      []
                  "ws" -> WS.runClientWith
                      (T.unpack host')
                      (extractPort relay)
                      (T.unpack $ extractPath relay)
                      options
                      []
                  _ -> \_ -> return ()  -- Do nothing for invalid scheme

            logDebug $ "Attempting to connect to " <> relayName relay
            connectionMVar <- newEmptyMVar
            errorMVar <- newEmptyMVar

            -- Fork a new thread to handle the connection
            _ <- forkIO $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
                result <- liftIO $ E.try @E.SomeException $ connectAction $ \conn -> runE $ do
                    -- Signal that the connection is established
                    putMVar connectionMVar conn
                    
                    logDebug $ "Connected to " <> relayName relay
                    modify @WebSocketState $ \st -> 
                      let updatedState = RelayConnectionState Connected 0
                      in st { connections = Map.insert (uri relay) updatedState (connections st) }
                    
                    -- Start receive and send loops
                    receiveThread <- async $ receiveWs relay conn responseQueue
                    sendThread <- async $ sendWs relay conn requestChan
                    
                    -- Wait for either thread to finish (which would indicate a disconnection)
                    _ <- waitAnyCancel [receiveThread, sendThread]
                    
                    logDebug $ "Disconnected from " <> relayName relay
                    modify @WebSocketState $ \st -> 
                      let updatedState = RelayConnectionState Disconnected 0
                      in st { connections = Map.insert (uri relay) updatedState (connections st) }

                case result of
                    Left _ -> runE $ do
                      logError $ "Connection error on " <> relayName relay
                      putMVar errorMVar ConnectionFailed
                    Right _ -> return ()

            -- Wait for either the connection to be established or an error to occur
            _ <- forkIO $ do
                threadDelay 15000000  -- 15 seconds timeout
                putMVar errorMVar Timeout

            result <- race (takeMVar connectionMVar) (takeMVar errorMVar)

            case result of
                Left _ -> return $ Right ()
                Right err -> return $ Left err

        _ -> do
            logError $ "Invalid relay configuration: " <> T.pack (show relay)
            return $ Left InvalidRelayConfig

-- | Receive messages from the relay.
receiveWs
  :: (WebSocketEff es, State WebSocketState :> es)
  => Relay
  -> WS.Connection
  -> TQueue Response
  -> Eff es ()
receiveWs relay conn responseQueue = do
    msgResult <- liftIO (E.try (WS.receiveData conn) :: IO (Either E.SomeException BSL.ByteString))
    case msgResult of
        Left e -> do
          handleConnectionError relay "Receive" e
          return ()
        Right msg ->
            case eitherDecode msg of
                Right response -> do
                    atomically $ writeTQueue responseQueue response
                    receiveWs relay conn responseQueue
                Left err -> do
                    logWarning $ "Could not decode server response: " <> T.pack err
                    --logWarning $ "Raw message: " <> T.pack (show msg)
                    receiveWs relay conn responseQueue


-- | Send messages to the relay.
sendWs 
  :: (WebSocketEff es, State WebSocketState :> es)
  => Relay
  -> WS.Connection
  -> TChan Request
  -> Eff es ()
sendWs relay conn channel = do
    msg <- atomically $ readTChan channel
    case msg of
        Nostr.Types.Disconnect -> do
            liftIO $ WS.sendClose conn (T.pack "Bye!")
            logDebug $ "Sent close message."
            sendWs relay conn channel
        msg' -> do
            result <- liftIO $ E.try @E.SomeException $ WS.sendTextData conn $ encode msg'
            case result of
                Left e -> do
                  handleConnectionError relay "Send" e
                  return ()
                Right _ -> do
                  logDebug $ "Sent message to relay " <> relayName relay
                  sendWs relay conn channel


-- | Handle connection errors
handleConnectionError
  :: (WebSocketEff es, State WebSocketState :> es)
  => Relay
  -> T.Text
  -> E.SomeException
  -> Eff es ()
handleConnectionError relay operation e = do
    logError $ operation <> " error for " <> relayName relay <> ": " <> T.pack (show e)
    modify @WebSocketState $ \st -> 
      let updatedState = RelayConnectionState Disconnected 0
      in st { connections = Map.insert (uri relay) updatedState (connections st) }
