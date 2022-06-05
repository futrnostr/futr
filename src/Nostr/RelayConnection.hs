{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Nostr.RelayConnection where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad (forever, mzero, unless, void)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Monomer
--import Network.Socket
import Network.WebSockets (ClientApp, Connection, receiveData,
                           sendClose, sendTextData)
import Text.URI (URI, render)
import Text.URI.Lens
import Wuss

import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Network.Connection as Connection
import qualified Network.HTTP.Req as Req
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream

import Nostr.Event
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response

connect
  :: WidgetEvent e
  => TChan Request
  -> Map SubscriptionId RelayHandler
  -> (e -> IO ())
  -> (Relay -> e)
  -> (Relay -> e)
  -> Relay
  -> IO ()
connect channel handlers sendMsg msgConnected msgDisconnected relay = do
  putStrLn $ "trying to connect to " ++ (unpack $ relayName relay) ++ " ..."
  start $ \conn -> do
    let relay' = relay { connected = True }
    putStrLn $ "Connected to " ++ (unpack $ relayName relay)
    sendMsg $ msgConnected relay'
    receiveWs sendMsg msgDisconnected relay' conn handlers
    sendWs channel sendMsg msgDisconnected relay' conn

--    receiveWs sendMsg msgDisconnected relay conn handlers
    --sendWs channel relay' conn sendMsg

    --if readable info' then receiveWs relay' conn sendMsg else return ()
--    if writable info' then sendWs channel relay' conn sendMsg else return ()
  where
    host = unpack $ extractHostname relay
    port = extractPort relay
    path = unpack $ extractPath relay
--    info' = info relay
    start = case extractScheme relay of
      "wss" -> runSecureClient host (fromIntegral port) path
      "ws"  -> WS.runClient host port path
      _     -> error "Wrong websocket scheme"

disconnect :: TChan Request -> Relay -> IO ()
disconnect channel relay =
  if not $ connected relay
    then return ()
    else atomically $ writeTChan channel $ Disconnect relay

receiveWs
  :: WidgetEvent e
  => (e -> IO ())
  -> (Relay -> e)
  -> Relay
  -> WS.Connection
  -> Map SubscriptionId RelayHandler
  -> IO ()
receiveWs sendMsg msgDisconnected relay conn handlers =
  if not $ readable $ info relay
    then return ()
    else void . forkIO $ void . runMaybeT $ forever $ do
      msg <- lift (Exception.try $ WS.receiveData conn :: IO (Either WS.ConnectionException LazyBytes.ByteString))
      case msg of
        Left ex    -> do
          liftIO $ putStrLn $ "Connection to " ++ (unpack $ relayName relay) ++ " closed"
          lift $ sendMsg $ msgDisconnected relay
          mzero
        Right msg' -> case decode msg' of
          Just m -> case m of
            EventReceived subId event ->
              case Map.lookup subId handlers of
                Just (RelayHandler handler) ->
                  handler subId event -- @todo does this work???
                Nothing ->
                  lift $ putStrLn $ "No event handler found for subscription " ++ unpack subId
            Notice notice ->
              lift $ putStrLn $ "NOTICE: " ++ unpack notice
          Nothing -> do
            lift $ putStrLn $ "Could not decode server response: " ++ show msg'

sendWs
  :: WidgetEvent e
  =>  TChan Request
  -> (e -> IO ())
  -> (Relay -> e)
  -> Relay
  -> WS.Connection
  -> IO ()
sendWs broadcastChannel sendMsg msgDisconnected relay conn =
  if not $ writable $ info relay
    then return ()
    else do
      channel <- atomically $ dupTChan broadcastChannel
      forever $ do
        msg <- Exception.try $ liftIO . atomically $ readTChan channel :: IO (Either WS.ConnectionException Request)
        case msg of
          Left ex -> sendMsg $ msgDisconnected relay
          Right msg' -> case msg' of
            Disconnect relay' ->
              if relay `sameRelay` relay' then do
                  WS.sendClose conn $ pack "Bye!"
              else return ()
            _ ->
              WS.sendTextData conn $ encode msg'
