{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module AppTypes where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr
import           Data.DateTime
import           Data.Default
import qualified Data.Map                             as Map
import           Data.Text
import           Monomer                              (WidgetEnv, WidgetNode)
import qualified Network.WebSockets                   as WS
import           Network.Socket

import           NostrTypes
import           Widgets.EditProfile
import           Widgets.ViewProfile

type AppWenv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

newtype AppEnv =
  AppEnv
    { _channel :: TChan ServerRequest
    }

data AppDialog
    = GenerateKeyPairDialog
    | ErrorReadingKeysFileDialog
    | NewRelayDialog
    | RelayDialog Relay
    deriving (Eq, Show)

data AppView
    = PostsView
    | PostDetailsView ReceivedEvent
    | ProfileView XOnlyPubKey
    | EditProfileView
    deriving (Eq, Show)

data RelayModel =
  RelayModel
    { _relayHostInput     :: Text
    , _relayPortInput     :: Integer
    , _relaySecureInput   :: Bool
    , _relayReadableInput :: Bool
    , _relayWritableInput :: Bool
    }
  deriving (Eq, Show)

instance Default RelayModel where
  def = RelayModel "" 433 True True True

data Subscription = Subscription Text DateTime
  deriving (Eq, Show)

instance Default Subscription where
  def = Subscription "" $ fromSeconds 0

data AppModel =
  AppModel
    { _time             :: DateTime
    , _keys             :: [Keys]
    , _selectedKeys     :: Maybe Keys
    , _following        :: Map.Map XOnlyPubKey [Profile]
    , _profiles         :: Map.Map XOnlyPubKey Profile
    , _currentSub       :: Subscription
    , _pool             :: [Relay]
    , _mySecKeyInput    :: Text
    , _newPostInput     :: Text
    , _receivedEvents   :: [ReceivedEvent]
    , _eventFilters     :: [EventFilter]
    , _dialog           :: Maybe AppDialog
    , _currentView      :: AppView
    , _editProfileModel :: EditProfileModel
    , _viewProfileModel :: ViewProfileModel
    , _relayModel       :: RelayModel
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel (fromSeconds 0) [] Nothing Map.empty Map.empty def defaultPool "" "" [] [] Nothing PostsView def def def

data AppEvent
  = AppInit
  | ConnectRelay Relay
  | DisconnectRelay Relay
  | UpdateRelay Relay
  | RelayConnected Relay
  | AddRelay
  | RelayDisconnected Relay
  | ShowDialog AppDialog
  | Subscribe [EventFilter]
  | Subscribed Text DateTime
  | KeyPairsLoaded [Keys]
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | KeysSelected (Maybe Keys)
  | NoKeysFound
  | ErrorReadingKeysFile
  | ImportSecKey
  | SendPost
  | ViewPostDetails ReceivedEvent
  | EditProfile
  | ViewProfile XOnlyPubKey
  | Back
  | PostSent
  | ReplyToPost Event
  | EventAppeared Event Relay
  | CloseDialog
  | TimerTick DateTime
  | NoOp
  deriving (Eq, Show)

makeLenses 'AppEnv
makeLenses 'RelayModel
makeLenses 'AppModel
