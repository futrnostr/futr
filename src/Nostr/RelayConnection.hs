{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Nostr.RelayConnection where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
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
  -> MVar RelayPool
  -> (e -> IO ())
  -> ([Relay] -> e)
  -> Relay
  -> IO ()
connect channel pool sendMsg msgRelaysUpdated relay = do
  putStrLn $ "trying to connect to " ++ (unpack $ relayName relay) ++ " ..."
  start $ \conn -> do
    let relay' = relay { connected = True }
    putStrLn $ "Connected to " ++ (unpack $ relayName relay)
    relays <- updateRelayPool pool relay True
    sendMsg $ msgRelaysUpdated relays
    receiveWs pool sendMsg msgRelaysUpdated relay' conn
    sendWs channel pool sendMsg msgRelaysUpdated relay' conn
  where
    host = unpack $ extractHostname relay
    port = extractPort relay
    path = unpack $ extractPath relay
    start = case extractScheme relay of
      "wss" -> runSecureClient host (fromIntegral port) path
      "ws"  -> WS.runClient host port path
      _     -> error "Wrong websocket scheme"

receiveWs
  :: WidgetEvent e
  => MVar RelayPool
  -> (e -> IO ())
  -> ([Relay] -> e)
  -> Relay
  -> WS.Connection
  -> IO ()
receiveWs pool sendMsg msgRelaysUpdated relay conn =
  if not $ readable $ info relay
    then return ()
    else void . forkIO $ void . runMaybeT $ forever $ do
      msg <- lift (Exception.try $ WS.receiveData conn :: IO (Either WS.ConnectionException LazyBytes.ByteString))
      case msg of
        Left ex -> do
          liftIO $ putStrLn $ "Connection to " ++ (unpack $ relayName relay) ++ " closed"
          relays <- liftIO $ updateRelayPool pool relay False
          lift $ sendMsg $ msgRelaysUpdated relays
          mzero
        Right msg' -> case decode msg' of
          Just (EventReceived subId event) -> do
            (RelayPool _ handlers) <- lift $ readMVar pool
            case Map.lookup subId handlers of
              Just responseChannel ->
                lift $ atomically $ writeTChan responseChannel $ (EventReceived subId event, relay)
              Nothing ->
                return ()
          Just (Notice notice) -> do
            lift $ putStrLn $ "Notice: " ++ unpack notice
            (RelayPool _ handlers) <- lift $ readMVar pool
            mapM_
              (\responseChannel -> lift $ atomically $ writeTChan responseChannel $ (Notice notice, relay))
              (Map.elems handlers)
          Nothing -> do
            lift $ putStrLn $ "Could not decode server response: " ++ show msg'

sendWs
  :: WidgetEvent e
  => TChan Request
  -> MVar RelayPool
  -> (e -> IO ())
  -> ([Relay] -> e) -- @todo add msg to send when all relays disconnected
  -> Relay
  -> WS.Connection
  -> IO ()
sendWs broadcastChannel pool sendMsg msgRelaysUpdated relay conn =
  if not $ writable $ info relay
    then return ()
    else do
      channel <- atomically $ dupTChan broadcastChannel
      forever $ do
        msg <- Exception.try $ liftIO . atomically $ readTChan channel :: IO (Either WS.ConnectionException Request)
        case msg of
          Left ex -> do
            relays <- liftIO $ updateRelayPool pool relay False
            liftIO $ sendMsg $ msgRelaysUpdated relays
            mzero
          Right msg' -> case msg' of
            Disconnect relay' ->
              if relay `sameRelay` relay' then do
                  WS.sendClose conn $ pack "Bye!"
              else return ()
            _ ->
              WS.sendTextData conn $ encode msg'

updateRelayPool :: MVar RelayPool -> Relay -> Bool -> IO [Relay]
updateRelayPool pool relay isConnected = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let relays' = map (\r -> if r `sameRelay` relay then r { connected = isConnected } else r) relays
  putMVar pool (RelayPool relays' responseChannels)
  return relays'
