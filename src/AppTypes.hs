{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module AppTypes where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr
import           Data.DateTime
import           Data.Default
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Text
import           Monomer                              (WidgetEnv, WidgetNode)
import qualified Network.WebSockets                   as WS
import           Network.Socket

import Helpers
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request (Request, SubscriptionId)
import Nostr.Response
import Widgets.BackupKeys
import Widgets.EditProfile
import Widgets.KeyManagement
import Widgets.Home
import Widgets.Setup

type AppWenv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

data AppEnv =
  AppEnv
    { _channel   :: TChan Request
    , _relayPool :: MVar RelayPool
    }

data AppView
    = HomeView
    | SetupView
    | BackupKeysView
    | EditProfileView
    | KeyManagementView
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
    { _keys              :: [Keys]
    , _selectedKeys      :: Keys
    , _myMetadataContent :: Maybe MetadataContent
    , _relays            :: [Relay]
    , _errorMsg          :: Maybe Text
    -- views
    , _currentView       :: AppView
    , _editProfileModel  :: EditProfileModel
    , _homeModel         :: HomeModel
    , _relayModel        :: RelayModel
    , _setupModel        :: SetupModel
    , _backupKeysModel   :: BackupKeysModel
    , _keyMgmtModel      :: KeyManagementModel
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel [] initialKeys Nothing [] Nothing HomeView def def def def def def

data AppEvent
  = NoOp
  | AppInit
  | GoHome
  | GoKeyManagement
  | GoSetup
  | RelaysInitialized [Relay]
  -- keys
  | KeyPairsLoaded [Keys]
  | NoKeysFound
  | ErrorReadingKeysFile
  | NewKeysCreated Keys MetadataContent
  | KeysBackupDone
  | KeysUpdated [Keys]
  -- relays
  | ConnectRelay Relay
  | DisconnectRelay Relay
  | RelayConnected Relay
  | RelayDisconnected Relay
  -- profile
  | EditProfile
  | ProfileUpdated MetadataContent
  {-
  | ValidateAndAddRelay
  | InvalidRelayURI
  | AddRelay Relay
  -}
  -- keys events
--  | KeysUpdated [Keys]
--  | KeysSelected (Keys)
  -- relay connection
  -- | TimerTick DateTime
  -- | Initialize
  -- | InitSubscribed SubscriptionId
  -- | HomeFilterSubscribed SubscriptionId
  -- | EventAppeared Event Relay

makeLenses 'AppEnv
makeLenses 'RelayModel
makeLenses 'AppModel
