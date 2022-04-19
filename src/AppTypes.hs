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
    { _channel :: TChan ServerRequest
    }

type Keys = (KeyPair, XOnlyPubKey)

data AppDialog
    = NoAppDialog
    | GenerateKeyPairDialog
    | ViewPostDialog
    deriving (Eq, Show)

data AppModel =
  AppModel
    { _keys           :: [Keys]
    , _currentKeys    :: Maybe Keys
    , _pool           :: [Relay]
    , _mySecKeyInput  :: Text
    , _newPostInput   :: Text
    , _receivedEvents :: [Event]
    , _eventFilter    :: Maybe EventFilter
    , _viewPost       :: Maybe Event
    , _dialog         :: AppDialog
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel [] Nothing [] "" "" [] Nothing Nothing NoAppDialog

data AppEvent
  = AppInit
  | RelayConnected Relay
  | AddRelay Relay
  | RelayDisconnected Relay
  | ShowGenerateKeyPairDialog
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | ImportSecKey
  | SendPost
  | ViewPost Event
  | Back
  | PostSent
  | ReplyToPost Event
  | EventAppeared Event
  | CloseDialog
  | NoOp
  deriving (Eq, Show)

makeLenses 'AppEnv
makeLenses 'AppModel
