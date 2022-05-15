{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module AppTypes where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr
import           Data.Default
import           Data.Text
import           Monomer                              (WidgetEnv, WidgetNode)
import qualified Network.WebSockets                   as WS
import           Network.Socket

import           NostrTypes
import           Widgets.Profile

type AppWenv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

newtype AppEnv =
  AppEnv
    { _channel :: TChan ServerRequest
    }

data AppDialog
    = NoAppDialog
    | GenerateKeyPairDialog
    | ViewPostDialog
    | ErrorReadingKeysFileDialog
    | NewRelayDialog
    | RelayDialog Relay
    deriving (Eq, Show)

data AppView
    = PostsView
    | ProfileView
    deriving (Eq, Show)

type ReceivedEvent = (Event, [Relay])

data AppModel =
  AppModel
    { _keys           :: [Keys]
    , _selectedKeys   :: Maybe Keys
    , _currentSub     :: Text
    , _pool           :: [Relay]
    , _mySecKeyInput  :: Text
    , _newPostInput   :: Text
    , _receivedEvents :: [ReceivedEvent]
    , _eventFilter    :: Maybe EventFilter
    , _viewPost       :: Maybe ReceivedEvent
    , _dialog         :: AppDialog
    , _currentView    :: AppView
    , _profileModel   :: ProfileModel
    , _relayHostInput :: Text
    , _relayPortInput :: Integer
    , _relaySecureInput   :: Bool
    , _relayReadableInput :: Bool
    , _relayWritableInput :: Bool
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel [] Nothing "" defaultPool "" "" [] Nothing Nothing NoAppDialog PostsView def "" 433 True True True

data AppEvent
  = AppInit
  | ConnectRelay Relay
  | DisconnectRelay Relay
  | UpdateRelay Relay
  | RelayConnected Relay
  | AddRelay
  | RelayDisconnected Relay
  | ShowDialog AppDialog
  | Subscribed Text
  | KeyPairsLoaded [Keys]
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | KeysSelected (Maybe Keys)
  | NoKeysFound
  | ErrorReadingKeysFile
  | ImportSecKey
  | SendPost
  | ViewPost ReceivedEvent
  | ViewProfile
  | Back
  | PostSent
  | ReplyToPost Event
  | EventAppeared Event Relay
  | CloseDialog
  | NoOp
  deriving (Eq, Show)

makeLenses 'AppEnv
makeLenses 'AppModel
