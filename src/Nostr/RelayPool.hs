{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Nostr.RelayPool where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Crypto.Random.DRBG (CtrDRBG, genBytes, newGen, newGenIO)
import Data.Aeson (encode)
import Data.Default
import Data.List (sort)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Monomer
import Wuss

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.URI.QQ as QQ

import Nostr.Event
import Nostr.Filter
import Nostr.Relay
import Nostr.Request
import Nostr.Response

data RelayPool = RelayPool [Relay] (Map SubscriptionId (TChan (Response, Relay)))

registerResponseChannel :: MVar RelayPool -> SubscriptionId -> TChan (Response, Relay) -> IO ()
registerResponseChannel pool subId responseChannel = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let responseChannels' = Map.insert subId responseChannel responseChannels
  putMVar pool (RelayPool relays responseChannels')

removeResponseChannel :: MVar RelayPool -> SubscriptionId -> IO ()
removeResponseChannel pool subId = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let responseChannels' = Map.delete subId responseChannels
  putMVar pool (RelayPool relays responseChannels')

addRelay :: MVar RelayPool -> Relay -> IO [Relay]
addRelay pool relay = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let relays' = sort $ relay : (filter (\r -> not $ r `sameRelay` relay) relays)
  putMVar pool (RelayPool relays' responseChannels)
  saveRelays relays'
  return relays'

removeRelay :: MVar RelayPool -> Relay -> IO [Relay]
removeRelay pool relay = do
  (RelayPool relays responseChannels) <- takeMVar pool
  let relays' = filter (\r -> not $ r `sameRelay` relay) relays
  putMVar pool (RelayPool relays' responseChannels)
  saveRelays relays'
  return relays'

saveRelays :: [Relay] -> IO ()
saveRelays relays = do
  LazyBytes.writeFile "relays.ft" $ encode relays
  putStrLn "Relays saved to disk"

subscribe :: MVar RelayPool -> TChan Request -> TChan (Response, Relay) -> [Filter] -> IO SubscriptionId
subscribe pool request response filters = do
  gen <- newGenIO :: IO CtrDRBG
  let Right (randomBytes, newGen) = genBytes 6 gen
  let subId = B16.encodeBase16 randomBytes
  registerResponseChannel pool subId response
  send request $ Subscribe $ Subscription filters subId
  return subId

unsubscribe :: MVar RelayPool -> TChan Request -> SubscriptionId -> IO ()
unsubscribe pool channel subId = do
  send channel $ Close subId
  removeResponseChannel pool subId

send :: TChan Request -> Request -> IO ()
send channel request =
  atomically $ writeTChan channel $ request

waitForActiveConnections :: MVar RelayPool -> Int -> IO ()
waitForActiveConnections pool timeout = do
  (RelayPool relays responseChannels) <- readMVar pool
  if and (map connected relays) || timeout <= 0
    then return ()
    else do
      threadDelay 100000
      waitForActiveConnections pool (timeout - 100000)
