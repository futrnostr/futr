{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes #-}

module Nostr.RelayPool where

import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Crypto.Random.DRBG (CtrDRBG, genBytes, newGen, newGenIO)
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Monomer
import Wuss

import qualified Data.ByteString.Base16 as B16
import qualified Data.Map as Map
import qualified Text.URI.QQ as QQ

import Nostr.Event
import Nostr.Filter
import Nostr.Relay
import Nostr.Request
import Nostr.Response

newtype RelayHandler = RelayHandler (forall i . Typeable i => SubscriptionId -> Event -> i)

data RelayPool = RelayPool [Relay] (Map SubscriptionId RelayHandler)

instance Default RelayPool where
  def =
    RelayPool
      [
      --   Relay
      --   { host = "nostr-pub.wellorder.net"
      --   , port = 443
      --   , secure = True
      --   , readable = True
      --   , writable = True
      --   , connected = False
      --   }
      -- ,
        Relay
        { uri = [QQ.uri|ws://localhost:2700|]
        , info = RelayInfo True True
        , connected = False
        }
      ]
      Map.empty

registerHandler :: RelayPool -> SubscriptionId -> RelayHandler -> RelayPool
registerHandler pool subId handler = do
   let RelayPool relays handlers = pool
   RelayPool relays (Map.insert subId handler handlers)

removeHandler :: RelayPool -> SubscriptionId -> RelayPool
removeHandler (RelayPool relays handlers) subId =
  RelayPool relays (Map.delete subId handlers)

addRelay :: RelayPool -> Relay -> RelayPool
addRelay (RelayPool relays handlers) relay =
  RelayPool (relay : (filter (\r -> r `sameRelay` relay) relays)) handlers

removeRelay :: RelayPool -> Relay -> RelayPool
removeRelay (RelayPool relays handlers) relay =
  RelayPool (filter (\r -> r `sameRelay` relay) relays) handlers

subscribe :: RelayPool -> TChan Request -> [Filter] -> RelayHandler -> IO (SubscriptionId, RelayPool)
subscribe pool channel filters handler = do
  gen <- newGenIO :: IO CtrDRBG
  let Right (randomBytes, newGen) = genBytes 16 gen
  let subId = B16.encodeBase16 randomBytes
  let pool' = registerHandler pool subId handler
  send channel $ Subscribe $ Subscription filters subId
  return (subId, pool')

unsubscribe :: RelayPool -> TChan Request -> SubscriptionId -> IO RelayPool
unsubscribe pool channel subId = do
  send channel $ Close subId
  return $ removeHandler pool subId

send :: TChan Request -> Request -> IO ()
send channel request =
  atomically $ writeTChan channel $ request
