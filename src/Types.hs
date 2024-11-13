module Types where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful.Concurrent.STM (TChan, TQueue)
import Graphics.QML (ObjRef)

import Nostr.Keys (KeyPair, PubKeyXO)
import Nostr.Types (Event, EventId, Filter, Profile, Relay, RelayURI, Request, SubscriptionId)


-- | UI updates
data UIUpdates = UIUpdates
  { profilesChanged :: Bool
  , followsChanged :: Bool
  , chatsChanged :: Bool
  , dmRelaysChanged :: Bool
  , generalRelaysChanged :: Bool
  , publishStatusChanged :: Bool
  , noticesChanged :: Bool
  } deriving (Eq, Show)


instance Semigroup UIUpdates where
  a <> b = UIUpdates
    { profilesChanged = profilesChanged a || profilesChanged b
    , followsChanged = followsChanged a || followsChanged b
    , chatsChanged = chatsChanged a || chatsChanged b
    , dmRelaysChanged = dmRelaysChanged a || dmRelaysChanged b
    , generalRelaysChanged = generalRelaysChanged a || generalRelaysChanged b
    , publishStatusChanged = publishStatusChanged a || publishStatusChanged b
    , noticesChanged = noticesChanged a || noticesChanged b
    }


instance Monoid UIUpdates where
  mempty = emptyUpdates


-- | Empty UI updates.
emptyUpdates :: UIUpdates
emptyUpdates = UIUpdates False False False False False False False


-- | Status of a publish operation
data PublishStatus
    = Publishing
    | WaitingForConfirmation
    | Success
    | Failure Text
    deriving (Eq, Show)


-- | Subscription events
data SubscriptionEvent
    = EventAppeared Event
    | SubscriptionEose
    | SubscriptionClosed Text


-- | State for RelayPool handling.
data RelayPoolState = RelayPoolState
    { activeConnections :: Map RelayURI RelayData
    , publishStatus :: Map EventId (Map RelayURI PublishStatus)
    , generalRelays :: Map PubKeyXO ([Relay], Int)
    , dmRelays :: Map PubKeyXO ([Relay], Int)
    }


-- | Subscription details.
data SubscriptionDetails = SubscriptionDetails
    { subscriptionId :: SubscriptionId
    , subscriptionFilters :: [Filter]
    , responseQueue :: TQueue SubscriptionEvent
    , eventsProcessed :: Int
    , newestCreatedAt :: Int
    }


-- | Connection errors.
data ConnectionError 
    = ConnectionFailed Text
    | AuthenticationFailed Text
    | NetworkError Text
    | TimeoutError
    | InvalidRelayConfig
    | MaxRetriesReached
    | UserDisconnected
    deriving (Show, Eq)


-- | Relay connection state.
data ConnectionState = Connected | Disconnected | Connecting
  deriving (Show, Eq)


-- | Data for each relay.
data RelayData = RelayData
  { connectionState :: ConnectionState
  , requestChannel :: TChan Request
  , activeSubscriptions :: Map SubscriptionId SubscriptionDetails
  , notices        :: [Text]
  , lastError      :: Maybe ConnectionError
  , connectionAttempts :: Int
  , pendingRequests :: [Request]
  , pendingEvents :: [Event]
  , pendingAuthId :: Maybe EventId
  }


-- | Initial state for RelayPool.
initialRelayPoolState :: RelayPoolState
initialRelayPoolState = RelayPoolState
  { activeConnections = Map.empty
  , publishStatus = Map.empty
  , generalRelays = Map.empty
  , dmRelays = Map.empty
  }


-- | Application screens
data AppScreen
    = KeyMgmt
    | Home
    deriving (Eq, Read, Show)


-- | Chat message.
data ChatMessage = ChatMessage
  { chatMessageId :: EventId
  , chatMessage :: Text
  , author :: PubKeyXO
  , chatMessageCreatedAt :: Int
  , timestamp :: Text
  } deriving (Show)


-- | Application state.
data AppState = AppState
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen
  -- Relay management
  , activeConnectionsCount :: Int
  -- Data storage
  , events :: Map EventId (Event, [Relay])
  , chats :: Map [PubKeyXO] [ChatMessage]
  , profiles :: Map PubKeyXO (Profile, Int)
  , follows :: Map PubKeyXO [Follow]
  -- UI state
  , currentChatRecipient :: (Maybe [PubKeyXO], Maybe SubscriptionId)
  , currentProfile :: Maybe PubKeyXO
  }

-- | UI object references grouped together
data UIReferences = UIReferences
  { profileObjRef :: Maybe (ObjRef ())
  , followsObjRef :: Maybe (ObjRef ())
  , chatObjRef :: Maybe (ObjRef ())
  , dmRelaysObjRef :: Maybe (ObjRef ())
  , generalRelaysObjRef :: Maybe (ObjRef ())
  }


-- | Follow.
data Follow = Follow
  { pubkey :: PubKeyXO
  , followRelay :: Maybe Relay
  , petName :: Maybe Text
  } deriving (Show)


-- | Initial application state.
initialState :: AppState
initialState = AppState
  { keyPair = Nothing
  , currentScreen = KeyMgmt
  , activeConnectionsCount = 0
  , events = Map.empty
  , chats = Map.empty
  , profiles = Map.empty
  , follows = Map.empty
  , currentChatRecipient = (Nothing, Nothing)
  , currentProfile = Nothing
  }
