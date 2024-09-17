{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Effects.WebSocket where

import Control.Monad (forever, void)
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (TChan, TQueue, atomically, readTChan, writeTQueue)
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

type WebSocketEff es = (Concurrent :> es, IOE :> es, Logging :> es)

-- | Handler for web socket effects.
runWebSocket
  :: forall (es :: [Effect]) a. WebSocketEff es
  => Eff (WebSocket : es) a
  -> Eff es a
runWebSocket = interpret $ \_ -> \case
  RunClient relay requestChan responseQueue -> case (extractHostname relay, extractScheme relay) of
    (Just host', Just scheme') ->
      let startClient = case scheme' of
            "wss" -> Wuss.runSecureClient (T.unpack host') (fromIntegral $ extractPort relay) (T.unpack $ extractPath relay)
            "ws"  -> WS.runClient (T.unpack host') (extractPort relay) (T.unpack $ extractPath relay)
            _     -> error "Unsupported websocket scheme"
      in withSeqEffToIO $ \runE -> startClient $ \conn -> runE $ do
        logDebug $ "Connected to " <> relayName relay
        void $ async $ receiveWs relay conn responseQueue
        void $ async $ sendWs relay conn requestChan

    _ -> logError $ "Invalid relay configuration: " <> T.pack (show relay)

-- Helper functions

receiveWs
  :: WebSocketEff es
  => Relay
  -> WS.Connection
  -> TQueue Response
  -> Eff es ()
receiveWs relay conn queue = forever $ do
  msg <- liftIO (Exception.try (WS.receiveData conn) :: IO (Either WS.ConnectionException BSL.ByteString))
  case msg of
    Left e -> do
      logInfo $ "Connection to " <> relayName relay <> " closed: " <> T.pack (show e)
    Right msg' -> 
      case decode msg' of
        Just response -> atomically $ writeTQueue queue response
        Nothing -> logInfo $ "Could not decode server response: " <> T.pack (show msg')

sendWs 
  :: WebSocketEff es
  => Relay
  -> WS.Connection
  -> TChan Request
  -> Eff es ()
sendWs relay conn channel = forever $ do
  msg <- atomically $ readTChan channel
  case msg of
      Nostr.Types.Disconnect -> liftIO $ WS.sendClose conn (T.pack "Bye!")
      _ -> liftIO $ WS.sendTextData conn $ encode msg
