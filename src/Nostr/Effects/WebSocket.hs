module Nostr.Effects.WebSocket where

import Control.Monad (forever, void)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (TChan, TQueue,atomically, readTChan, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import Control.Exception qualified as Exception
import Network.WebSockets qualified as WS
import Wuss qualified as Wuss

import Nostr.Effects.Logging
import Nostr.Relay
import Nostr.Types

-- | Effect for handling WebSocket operations.
data WebSocket :: Effect where
    RunClient :: Relay -> TChan Request -> TQueue Response -> WebSocket m ()

type instance DispatchOf WebSocket = Dynamic

makeEffect ''WebSocket

-- | Effect for handling WebSocket operations.
type WebSocketEff es = (Concurrent :> es, IOE :> es, Logging :> es)

-- | Handler for web socket effects.
runWebSocket
  :: WebSocketEff es
  => Eff (WebSocket : es) a
  -> Eff es a
runWebSocket = interpret $ \_ -> \case
   RunClient relay requestChan responseQueue -> case (extractHostname relay, extractScheme relay) of
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
          void $ async $ receiveWs relay conn responseQueue
          sendWs relay conn requestChan)
          (\e -> runE $ logError $ "Failed to connect to " <> relayName relay <> ": " <> T.pack (show (e :: Exception.SomeException)))
    _ -> logError $ "Invalid relay configuration: " <> T.pack (show relay)

-- Helper functions

receiveWs
  :: WebSocketEff es
  => Relay
  -> WS.Connection
  -> TQueue Response
  -> Eff es ()
receiveWs relay conn responseQueue = forever $ do
  msg <- liftIO (Exception.try (WS.receiveData conn) :: IO (Either WS.ConnectionException BSL.ByteString))
  case msg of
    Left e -> do
      logInfo $ "Connection to " <> relayName relay <> " closed: " <> T.pack (show e)
      return ()
    Right msg' -> 
      case eitherDecode msg' of
        Right response -> do
          --logDebug $ "Received response: " <> T.pack (show response)
          atomically $ writeTQueue responseQueue response
        Left err -> do
          logWarning $ "Could not decode server response: " <> T.pack err
          logWarning $ "Raw message: " <> T.pack (show msg')

sendWs 
  :: WebSocketEff es
  => Relay
  -> WS.Connection
  -> TChan Request
  -> Eff es ()
sendWs relay conn channel = forever $ do
  msg <- atomically $ readTChan channel
  case msg of
    Nostr.Types.Disconnect -> do
      liftIO $ WS.sendClose conn (T.pack "Bye!")
      logDebug $ "Sent close msg to: " <> relayName relay
    _ -> do
      logDebug $ "Sending message: " <> T.pack (show msg)
      result <- liftIO $ Exception.try $ WS.sendTextData conn $ encode msg
      case result of
        Left e -> do
          logError $ "Failed to send message to " <> relayName relay <> ": " <> T.pack (show (e :: Exception.SomeException))
        Right _ -> do
          logDebug $ "Sent message to: " <> relayName relay
