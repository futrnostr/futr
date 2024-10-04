module Nostr.Effects.WebSocket where

import Control.Monad (void, when)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (TChan, TQueue, atomically, readTChan, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Control.Exception qualified as Exception
import Network.WebSockets qualified as WS
import Wuss qualified as Wuss
import Data.Map.Strict qualified as Map

import AppState (RelayPoolState(..), RelayData(..))
import Nostr.Effects.Logging
import Nostr.Types


-- | Effect for handling WebSocket operations.
data WebSocket :: Effect where
    RunClient :: Relay -> TChan Request -> TQueue Response -> WebSocket m ()


-- | Dispatch for WebSocket.
type instance DispatchOf WebSocket = Dynamic

makeEffect ''WebSocket


-- | Effect for handling WebSocket operations.
type WebSocketEff es = (Concurrent :> es, IOE :> es, Logging :> es, State RelayPoolState :> es)


-- | Handler for web socket effects.
runWebSocket
  :: WebSocketEff es
  => Eff (WebSocket : es) a
  -> Eff es a
runWebSocket = interpret $ \_ -> \case
   RunClient relay requestChan responseQueue' -> do
    relayPoolState <- get @RelayPoolState
    let relayData = Map.lookup (uri relay) (relays relayPoolState)
    case relayData of
      Just rd | retryCount rd >= 5 ->
        logDebug $ "Max retry attempts reached for " <> relayName relay
      _ -> do
        when (maybe True (\rd -> not (connected rd)) relayData) $
          modify @RelayPoolState $ \rps -> rps
            { relays = Map.alter (updateRelayData relay requestChan responseQueue') (uri relay) (relays rps) }
        connectToRelay relay requestChan responseQueue'


-- | Update the relay data.
updateRelayData :: Relay -> TChan Request -> TQueue Response -> Maybe RelayData -> Maybe RelayData
updateRelayData relay reqChan respQueue = \case
  Just rd -> Just rd { retryCount = retryCount rd + 1, requestChannel = reqChan, responseQueue = respQueue }
  Nothing -> Just $ RelayData False (info relay) reqChan respQueue [] [] 0


-- | Connect to a relay.
connectToRelay :: WebSocketEff es => Relay -> TChan Request -> TQueue Response -> Eff es ()
connectToRelay relay requestChan responseQueue' = case (extractHostname relay, extractScheme relay) of
  (Just host', Just scheme') ->
    let options = WS.defaultConnectionOptions { WS.connectionCompressionOptions = WS.PermessageDeflateCompression WS.defaultPermessageDeflate }
        startClient = case scheme' of
          "wss" -> Wuss.runSecureClientWith
            (T.unpack host')
            (fromIntegral $ extractPort relay)
            (T.unpack $ extractPath relay)
            options
            []
          "ws"  -> WS.runClientWith
            (T.unpack host')
            (extractPort relay)
            (T.unpack $ extractPath relay)
            options
            []
          _     -> error "Unsupported websocket scheme"
    in withSeqEffToIO $ \runE -> do
      runE $ logDebug $ "Attempting to connect to " <> relayName relay
      Exception.catch (startClient $ \conn -> runE $ do
        logDebug $ "Connected to " <> relayName relay
        modify @RelayPoolState $ \rps -> rps
          { relays = Map.adjust (\rd -> rd { connected = True, retryCount = 0 }) (uri relay) (relays rps) }
        void $ async $ receiveWs relay conn responseQueue'
        sendWs relay conn requestChan)
        (\e -> runE $ do
          logError $ "Failed to connect to " <> relayName relay <> ": " <> T.pack (show (e :: Exception.SomeException))
          modify @RelayPoolState $ \rps -> rps
            { relays = Map.adjust (\rd -> rd { connected = False }) (uri relay) (relays rps) })
  _ -> logError $ "Invalid relay configuration: " <> T.pack (show relay)


-- | Receive messages from the relay.
receiveWs
  :: WebSocketEff es
  => Relay
  -> WS.Connection
  -> TQueue Response
  -> Eff es ()
receiveWs relay conn responseQueue' = do
  let loop = do
        relayPoolState <- get @RelayPoolState
        case Map.lookup (uri relay) (relays relayPoolState) of
          Just relayData | connected relayData -> do
            msg <- liftIO (Exception.try (WS.receiveData conn) :: IO (Either WS.ConnectionException BSL.ByteString))
            case msg of
              Left e -> do
                logInfo $ "Connection to " <> relayName relay <> " closed: " <> T.pack (show e)
                modify @RelayPoolState $ \rps -> rps
                  { relays = Map.adjust (\rd -> rd { connected = False }) (uri relay) (relays rps) }
              Right msg' ->
                case eitherDecode msg' of
                  Right response -> do
                    atomically $ writeTQueue responseQueue' response
                    loop
                  Left err -> do
                    logWarning $ "Could not decode server response: " <> T.pack err
                    logWarning $ "Raw message: " <> T.pack (show msg')
                    loop
          _ -> logDebug $ "Receive loop stopped for " <> relayName relay
  loop


-- | Send messages to the relay if writable.
sendWs 
  :: WebSocketEff es
  => Relay
  -> WS.Connection
  -> TChan Request
  -> Eff es ()
sendWs relay conn channel = loop
  where
    loop = do
      msg <- atomically $ readTChan channel
      case msg of
        Nostr.Types.SendEvent _ -> do
          when (writable $ info relay) $
            doSend relay conn msg
          loop

        Nostr.Types.Subscribe _ -> do
          when (readable $ info relay) $
            doSend relay conn msg
          loop

        Nostr.Types.Close _ -> do
          doSend relay conn msg
          loop

        Nostr.Types.Disconnect -> do
          liftIO $ WS.sendClose conn (T.pack "Bye!")
          logDebug $ "Sent close msg to: " <> relayName relay
          modify @RelayPoolState $ \rps -> rps
            { relays = Map.adjust (\rd -> rd { connected = False, retryCount = 0 }) (uri relay) (relays rps) }
          -- Exit the loop after Disconnect


-- | Actually send a message to the relay.
doSend :: WebSocketEff es => Relay -> WS.Connection -> Request -> Eff es ()
doSend relay conn msg = do
  result <- liftIO $ Exception.try $ WS.sendTextData conn $ encode msg
  case result of
    Left e -> do
      logError $ "Failed to send message to " <> relayName relay <> ": " <> T.pack (show (e :: Exception.SomeException))
      return ()
    Right _ -> do
      logDebug $ "Sent message to " <> relayName relay
      return ()
