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

import           Nostr.Event
import           Nostr.Filter
import           Nostr.Keys
import           Nostr.Profile
import           Nostr.Relay
import           Nostr.Request       (Request, SubscriptionId)
import           Widgets.EditProfile
import           Widgets.Home
import           Widgets.ViewPosts
import           Widgets.ViewProfile

type AppWenv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

newtype AppEnv =
  AppEnv
    { _channel :: TChan Request
    }

data AppDialog
    = GenerateKeyPairDialog
    | ErrorReadingKeysFileDialog
    | NewRelayDialog
    | RelayDialog Relay
--    | DeleteEventDialog Event
    deriving (Eq, Show)

data AppView
    = HomeView
    | SetupView
    | EditProfileView
    deriving (Eq, Show)

data RelayModel =
  RelayModel
    { _relayURI           :: Text
    , _relayReadableInput :: Bool
    , _relayWritableInput :: Bool
    , _isInvalidInput     :: Bool
    }
  deriving (Eq, Show)

instance Default RelayModel where
  def = RelayModel "wss://" True True False

data AppModel =
  AppModel
    { _time             :: DateTime
    , _keys             :: [Keys]
    , _selectedKeys     :: Maybe Keys
    , _mainSub          :: SubscriptionId
    , _contactsSub      :: SubscriptionId
    , _initialSub       :: SubscriptionId
    , _pool             :: [Relay]
    , _mySecKeyInput    :: Text
--    , _searchInput      :: Text
    , _dialog           :: Maybe AppDialog
    , _currentView      :: AppView
    , _editProfileModel :: EditProfileModel
    , _homeModel        :: HomeModel
    , _viewProfileModel :: ViewProfileModel
    , _relayModel       :: RelayModel
    , _deleteReason     :: Text
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel (fromSeconds 0) [] Nothing "" "" "" defaultPool "" Nothing HomeView def def def def defaultDeleteReason

defaultDeleteReason :: Text
defaultDeleteReason = "This post was published by accident."

data AppEvent
  = AppInit
  | ConnectRelay Relay
  | DisconnectRelay Relay
  | UpdateRelay Relay
  | RelayConnected Relay
  | ValidateAndAddRelay
  | InvalidRelayURI
  | AddRelay Relay
  | RelayDisconnected Relay
  | ShowDialog AppDialog
  | KeyPairsLoaded [Keys]
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | KeysSelected (Maybe Keys)
  | NoKeysFound
  | ErrorReadingKeysFile
  | ImportSecKey
  | EditProfile
--  | SearchProfile Text
--  | Back
  -- | EventSent
  -- | ReplyToPost Event
  -- | DeleteEvent Event
  -- | EventAppeared Event Relay
  | CloseDialog
  | TimerTick DateTime
  | NoOp
  deriving (Eq, Show)

makeLenses 'AppEnv
makeLenses 'RelayModel
makeLenses 'AppModel
