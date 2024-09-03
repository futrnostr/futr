{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Nostr.RelayPool where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16


import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import qualified Data.Text.Encoding as TE


import System.Random (newStdGen, randoms)

import Nostr.Filter
import Nostr.Relay
import Nostr.Request
import Nostr.Response hiding (Close)

data RelayPool = RelayPool [Relay] (Map SubscriptionId (TChan (Response, Relay)))

registerResponseChannel :: MVar RelayPool -> SubscriptionId -> TChan (Response, Relay) -> IO ()
registerResponseChannel pool subId' responseChannel = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let responseChannels' = Map.insert subId' responseChannel responseChannels
  putMVar pool (RelayPool relays responseChannels')

removeResponseChannel :: MVar RelayPool -> SubscriptionId -> IO ()
removeResponseChannel pool subId' = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let responseChannels' = Map.delete subId' responseChannels
  putMVar pool (RelayPool relays responseChannels')

addRelay :: MVar RelayPool -> Relay -> IO [Relay]
addRelay pool relay = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let relays' = sort $ relay : (filter (\r -> not $ r `sameRelay` relay) relays)
  putMVar pool (RelayPool relays' responseChannels)
  return relays'

removeRelay :: MVar RelayPool -> Relay -> IO [Relay]
removeRelay pool relay = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let relays' = filter (\r -> not $ r `sameRelay` relay) relays
  putMVar pool (RelayPool relays' responseChannels)
  return relays'

subscribe :: MVar RelayPool -> TChan Request -> TChan (Response, Relay) -> [Filter] -> IO SubscriptionId
subscribe pool request response filters' = do
  randomBytes <- BS.pack . take 16 . randoms <$> newStdGen
  let subId' = TE.decodeUtf8 (B16.encode randomBytes)
  registerResponseChannel pool subId' response
  send request $ Subscribe $ Subscription filters' subId'
  return subId'

unsubscribe :: MVar RelayPool -> TChan Request -> SubscriptionId -> IO ()
unsubscribe pool channel subId' = do
  send channel $ Close subId'
  removeResponseChannel pool subId'

send :: TChan Request -> Request -> IO ()
send channel request =
  atomically $ writeTChan channel $ request

waitForActiveConnections :: MVar RelayPool -> Int -> IO ()
waitForActiveConnections pool timeout = do
  (RelayPool relays _) <- readMVar pool
  if and (map connected relays) || timeout <= 0
    then return ()
    else do
      threadDelay 100000
      waitForActiveConnections pool (timeout - 100000)
