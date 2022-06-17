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
import qualified Text.URI.QQ as QQ

import Nostr.Event
import Nostr.Filter
import Nostr.Relay
import Nostr.Request
import Nostr.Response

data RelayPool = RelayPool [Relay] (Map SubscriptionId (TChan Response))

registerResponseChannel :: MVar RelayPool -> SubscriptionId -> TChan Response -> IO ()
registerResponseChannel poolMVar subId responseChannel = do
  (RelayPool relays responseChannels) <- takeMVar poolMVar
  let responseChannels' = Map.insert subId responseChannel responseChannels
  putMVar poolMVar (RelayPool relays responseChannels')

removeResponseChannel :: MVar RelayPool -> SubscriptionId -> IO ()
removeResponseChannel poolMVar subId = do
  (RelayPool relays responseChannels) <- takeMVar poolMVar
  let responseChannels' = Map.delete subId responseChannels
  putMVar poolMVar (RelayPool relays responseChannels')

addRelay :: MVar RelayPool -> Relay -> IO [Relay]
addRelay poolMVar relay = do
  (RelayPool relays responseChannels) <- takeMVar poolMVar
  let relays' = sort $ relay : (filter (\r -> not $ r `sameRelay` relay) relays)
  putMVar poolMVar (RelayPool relays' responseChannels)
  LazyBytes.writeFile "relays.ft" $ encode relays'
  putStrLn "Relays saved to disk"
  return relays'

removeRelay :: MVar RelayPool -> Relay -> IO [Relay]
removeRelay poolMVar relay = do
  (RelayPool relays responseChannels) <- takeMVar poolMVar
  let relays' = filter (\r -> not $ r `sameRelay` relay) relays
  putMVar poolMVar (RelayPool relays' responseChannels)
  LazyBytes.writeFile "relays.ft" $ encode relays'
  putStrLn "Relays saved to disk"
  return relays'

subscribe :: MVar RelayPool -> TChan Request -> TChan Response -> [Filter] -> IO SubscriptionId
subscribe poolMVar request response filters = do
  gen <- newGenIO :: IO CtrDRBG
  let Right (randomBytes, newGen) = genBytes 16 gen
  let subId = B16.encodeBase16 randomBytes
  registerResponseChannel poolMVar subId response
  send request $ Subscribe $ Subscription filters subId
  return subId

unsubscribe :: MVar RelayPool -> TChan Request -> SubscriptionId -> IO ()
unsubscribe poolMVar channel subId = do
  send channel $ Close subId
  removeResponseChannel poolMVar subId

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
