{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module AppTypes where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr
import           Data.Default
import           Data.Text
import qualified Network.WebSockets                   as WS
import           Network.Socket

import           NostrTypes

newtype AppEnv =
  AppEnv
    { _channel :: TChan Event
    }

data AppModel =
  AppModel
    { _clickCount     :: Int
    , _myKeyPair      :: Maybe KeyPair
    , _myXOnlyPubKey  :: Maybe XOnlyPubKey
    , _pool           :: [Relay]
    , _mySecKeyInput  :: Text
    , _newPostInput   :: Text
    , _receivedEvents :: [Event]
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel 0 Nothing Nothing [] "" "" []

data AppEvent
  = AppInit
  | RelayConnected Relay
  | AddRelay Relay
  | AppIncrease
  | RelayDisconnected Relay
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | ImportSecKey
  | SendPost
  | ReplyToPost Event
  | EventAppeared Event
  | NoOp
  deriving (Eq, Show)

makeLenses 'AppEnv
makeLenses 'AppModel
