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
import Nostr.Types

data WebSocketConnArgs = WebSocketConnArgs
    { host :: String
    , port :: Int
    , path :: String
    , scheme :: String
    }

-- | Effect for handling WebSocket operations.
data WebSocket :: Effect where
    RunClient :: TChan Request -> TQueue Response -> WebSocketConnArgs -> WebSocket m ()

type instance DispatchOf WebSocket = Dynamic

makeEffect ''WebSocket

-- | Handler for web socket effects.
runWebSocket
  :: forall (es :: [Effect]) a.
     (Concurrent :> es, IOE :> es, Logging :> es)
  => Eff (WebSocket : es) a
  -> Eff es a
runWebSocket = interpret $ \_ -> \case
  RunClient requestChan responseQueue args ->
    let relayName = T.pack $ (host args) <> ":" <> (show $ port args) <> (path args)
        startClient = case scheme args of
          "wss" -> Wuss.runSecureClient (host args) (fromIntegral $ port args) (path args)
          "ws"  -> WS.runClient (host args) (port args) (path args)
          _     -> error "Unsupported websocket scheme" in
    withSeqEffToIO $ \runE -> startClient $ \conn -> runE $ do
      logDebug $ "Connected to " <> T.pack (host args)
      void $ async $ receiveWs relayName conn responseQueue
      void $ async $ sendWs conn requestChan

-- Helper functions

receiveWs
  :: (Concurrent :> es, IOE :> es, Logging :> es)
  => Text
  -> WS.Connection
  -> TQueue Response
  -> Eff es ()
receiveWs relayName conn queue = forever $ do
  msg <- liftIO (Exception.try (WS.receiveData conn) :: IO (Either WS.ConnectionException BSL.ByteString))
  case msg of
    Left e -> do
      logInfo $ "Connection to " <> relayName <> " closed: " <> T.pack (show e)
    Right msg' -> 
      case decode msg' of
        Just response -> atomically $ writeTQueue queue response
        Nothing -> logError $ "Could not decode server response: " <> T.pack (show msg')

sendWs 
  :: (Concurrent :> es, IOE :> es)
  => WS.Connection
  -> TChan Request
  -> Eff es ()
sendWs conn channel = forever $ do
  msg <- atomically $ readTChan channel
  case msg of
      Nostr.Types.Disconnect -> liftIO $ WS.sendClose conn (T.pack "Bye!")
      _ -> liftIO $ WS.sendTextData conn $ encode msg
