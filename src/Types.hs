module Types where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful.Concurrent.STM (TChan, TQueue)
import Nostr.Keys (KeyPair, PubKeyXO)
import Nostr.Types (Event, EventId, Filter, Profile, Relay, RelayURI, Request, SubscriptionId)


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


-- | Type of note
data NoteType
  = ShortTextNote
  | Repost EventId             -- kind 6, references original note
  | QuoteRepost EventId      -- kind 1 with q tag, includes quoted event and additional content
  | Comment {
      rootScope :: EventId,    -- root event being commented on
      rootKind :: Int,         -- kind of root event
      parentId :: EventId,     -- immediate parent (same as root for top-level comments)
      parentKind :: Int        -- kind of parent
    }
  deriving (Show, Eq)


-- | Simplified note reference that proxies most data through events map
data Post = Post
  { postId :: EventId          -- ID of this post
  , postType :: NoteType       -- Type of post and its references
  , postCreatedAt :: Int       -- Creation timestamp
  } deriving (Show)


-- | Application state.
data AppState = AppState
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen
  -- Data storage
  , posts :: Map PubKeyXO [Post]
  , events :: Map EventId (Event, [RelayURI])
  , chats :: Map PubKeyXO [ChatMessage]
  , profiles :: Map PubKeyXO (Profile, Int)
  , follows :: Map PubKeyXO [Follow]
  -- UI state
  , currentContact :: (Maybe PubKeyXO, Maybe SubscriptionId)
  , currentProfile :: Maybe PubKeyXO
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
  , events = Map.empty
  , posts = Map.empty
  , chats = Map.empty
  , profiles = Map.empty
  , follows = Map.empty
  , currentContact = (Nothing, Nothing)
  , currentProfile = Nothing
  }
