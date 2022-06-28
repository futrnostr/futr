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

import Futr
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
import Widgets.PostDetails
import Widgets.RelayManagement
import Widgets.Setup
import Widgets.Profile

type AppWenv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

data AppEnv =
  AppEnv
    { _pool    :: MVar RelayPool
    , _request :: TChan Request
    }

data AppView
    = HomeView
    | SetupView
    | BackupKeysView
    | EditProfileView
    | KeyManagementView
    | RelayManagementView
    | PostDetailsView
    | ProfileView
    deriving (Eq, Show)

data AppModel =
  AppModel
    { _keys              :: [Keys]
    , _futr              :: FutrModel
    , _inputField        :: Text
    , _searchInput       :: Text
    , _relays            :: [Relay]
    , _subscriptionId    :: Maybe SubscriptionId
    , _errorMsg          :: Maybe Text
    , _waitingForConns   :: Bool
    -- views
    , _currentView       :: AppView
    , _editProfileModel  :: EditProfileModel
    , _profileModel      :: ProfileModel
    , _postDetailsModel  :: PostDetailsModel
    , _relayModel        :: RelayModel
    , _setupModel        :: SetupModel
    , _backupKeysModel   :: BackupKeysModel
    , _keyMgmtModel      :: KeyManagementModel
    , _relayMgmtModel    :: RelayManagementModel
    }
  deriving (Eq, Show)

instance Default AppModel where
  def = AppModel [] def "" "" [] Nothing Nothing True HomeView def def def def def def def def

data AppEvent
  = NoOp
  | AppInit
  | RelaysInitialized [Relay]
  | TimerTick DateTime
  -- subscriptions
  | InitSubscriptions
  | SubscriptionsInitialized (Map XOnlyPubKey (Profile, DateTime))
  | SubscriptionStarted SubscriptionId
  | NewResponses [(Response, Relay)]
  | Dispose
  -- actions
  | SendPost Text
  | ReplyToPost Event Text
  | ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  | Follow XOnlyPubKey
  | Unfollow XOnlyPubKey
  | Search
  -- go to
  | GoHome
  | GoKeyManagement
  | GoRelayManagement
  | GoSetup
  -- keys
  | KeyPairsLoaded [Keys]
  | NoKeysFound
  | ErrorReadingKeysFile
  | NewKeysCreated Keys Profile DateTime
  | KeysBackupDone
  | KeysUpdated [Keys]
  -- relays
  | ConnectRelay Relay
  | RelaysUpdated [Relay]
  -- profile
  | EditProfile
  | ProfileUpdated Keys Profile DateTime
  deriving Show

makeLenses 'AppEnv
makeLenses 'RelayModel
makeLenses 'AppModel
