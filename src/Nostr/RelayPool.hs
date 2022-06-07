{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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

data RelayPool = RelayPool [Relay] (Map SubscriptionId (TChan Response))

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

registerResponseChannel :: RelayPool -> SubscriptionId -> TChan Response -> RelayPool
registerResponseChannel (RelayPool relays outputs) subId output =
   RelayPool relays (Map.insert subId output outputs)

removeResponseChannel :: RelayPool -> SubscriptionId -> RelayPool
removeResponseChannel (RelayPool relays outputs) subId =
  RelayPool relays (Map.delete subId outputs)

addRelay :: RelayPool -> Relay -> RelayPool
addRelay (RelayPool relays handlers) relay =
  RelayPool (relay : (filter (\r -> r `sameRelay` relay) relays)) handlers

removeRelay :: RelayPool -> Relay -> RelayPool
removeRelay (RelayPool relays handlers) relay =
  RelayPool (filter (\r -> r `sameRelay` relay) relays) handlers

subscribe :: RelayPool -> TChan Request -> [Filter] -> TChan Response -> IO (SubscriptionId, RelayPool)
subscribe pool input filters output = do
  gen <- newGenIO :: IO CtrDRBG
  let Right (randomBytes, newGen) = genBytes 16 gen
  let subId = B16.encodeBase16 randomBytes
  let pool' = registerResponseChannel pool subId output
  send input $ Subscribe $ Subscription filters subId
  return (subId, pool')

unsubscribe :: RelayPool -> TChan Request -> SubscriptionId -> IO RelayPool
unsubscribe pool channel subId = do
  send channel $ Close subId
  return $ removeResponseChannel pool subId

send :: TChan Request -> Request -> IO ()
send channel request =
  atomically $ writeTChan channel $ request
