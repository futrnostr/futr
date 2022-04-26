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

type Keys = (KeyPair, XOnlyPubKey, Bool)

data AppDialog
    = NoAppDialog
    | GenerateKeyPairDialog
    | ViewPostDialog
    | ErrorReadingKeysFileDialog
    | NewRelayDialog
    | RelayDialog Relay
    deriving (Eq, Show)

data AppModel =
  AppModel
    { _keys           :: [Keys]
    , _pool           :: [Relay]
    , _mySecKeyInput  :: Text
    , _newPostInput   :: Text
    , _receivedEvents :: [Event]
    , _eventFilter    :: Maybe EventFilter
    , _viewPost       :: Maybe Event
    , _dialog         :: AppDialog
    , _relayHostInput :: Text
    , _relayPortInput :: Integer
    , _relaySecureInput   :: Bool
    , _relayReadableInput :: Bool
    , _relayWritableInput :: Bool
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel [] defaultPool "" "" [] Nothing Nothing NoAppDialog "" 433 True True True

data AppEvent
  = AppInit
  | ConnectRelay Relay
  | DisconnectRelay Relay
  | UpdateRelay Relay
  | RelayConnected Relay
  | AddRelay
  | RelayDisconnected Relay
  | ShowRelayDialog Relay
  | AddNewRelayDialog
  | ShowGenerateKeyPairDialog
  | KeyPairsLoaded [Keys]
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | NoKeysFound
  | ErrorReadingKeysFile
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
