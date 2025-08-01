{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, (.:), (.=), withObject, object)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text, pack)
import Control.Concurrent.Async (Async)
import Effectful.Concurrent.STM (TChan, TQueue)
import GHC.Generics (Generic)
import Nostr.Event (Event, EventId)
import Nostr.Keys (KeyPair, PubKeyXO)
import Nostr.Types (Filter, Request, SubscriptionId)
import Nostr.Relay (RelayURI)
import Version (runtimeVersion)


-- | Language options for the application
data Language = English | Spanish | German
    deriving (Eq, Show)


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
    | SubscriptionEose SubscriptionId
    | SubscriptionClosed Text
    deriving (Show)


-- | State for RelayPool handling.
data RelayPool = RelayPool
    { activeConnections :: Map RelayURI RelayData
    , subscriptions :: Map SubscriptionId SubscriptionState
    , pendingSubscriptions :: Map SubscriptionId SubscriptionState
    , publishStatus :: Map EventId (Map RelayURI PublishStatus)
    , updateQueue :: TQueue ()
    , updateThread :: Maybe (Async ())
    , commentSubscriptions :: Map EventId [SubscriptionId]
    }


-- | Subscription details.
data SubscriptionState = SubscriptionState
    { subscriptionFilter :: Filter
    , responseQueue :: TQueue (RelayURI, SubscriptionEvent)
    , relay :: RelayURI
    , eventsProcessed :: Int
    , oldestCreatedAt :: Int
    }


-- | Create a new subscription state.
newSubscriptionState :: Filter -> TQueue (RelayURI, SubscriptionEvent) -> RelayURI -> SubscriptionState
newSubscriptionState f q r = SubscriptionState f q r 0 (maxBound :: Int)


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
  , notices        :: [Text]
  , lastError      :: Maybe ConnectionError
  , connectionAttempts :: Int
  , pendingRequests :: [Request]
  , pendingEvents :: [Event]
  , pendingAuthId :: Maybe EventId
  }


-- | Initial state for RelayPool.
initialRelayPool :: RelayPool
initialRelayPool = RelayPool
  { activeConnections = Map.empty
  , subscriptions = Map.empty
  , pendingSubscriptions = Map.empty
  , publishStatus = Map.empty
  , updateQueue = undefined
  , updateThread = Nothing
  , commentSubscriptions = Map.empty
  }


-- | Application screens
data AppScreen
    = KeyMgmt
    | Home
    deriving (Eq, Read, Show)


-- | Data type to store event with its relay sources
data EventWithRelays = EventWithRelays
    { event :: Event
    , relays :: Set RelayURI
    } deriving (Show, Generic, ToJSON, FromJSON)


data InboxModelState
  = Stopped
  | InitialBootstrap    -- ^ Setting up initial relay connections and configuration
  | SyncingHistoricData -- ^ Downloading and processing historical events
  | LiveProcessing      -- ^ Bootstrap complete, processing real-time events
  deriving (Eq, Show)


-- | Feed filter.
-- @todo refactor to be original filter?
data FeedFilter
  = PostsFilter PubKeyXO
  | PrivateMessagesFilter PubKeyXO
  | CommentsFilter EventId
  deriving (Eq, Show)


data Post = Post
  {
    postId :: EventId
  , postAuthor :: PubKeyXO
  , postEvent :: Event
  , postContent :: Text
  , postTimestamp :: Text
  , postType :: Text
  , referencedPostId :: Maybe EventId
  } deriving (Eq, Show)


-- | Feed.
data Feed = Feed
  { feedEvents :: [Post]
  , feedEventMap :: Map EventId Post
  , feedFilter :: FeedFilter
  } deriving (Eq, Show)


-- | Application state.
data AppState = AppState
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen -- @todo remove maybe?
  , currentFeed :: Maybe Feed
  , currentCommentFeed :: Maybe Feed
  , currentProfile :: Maybe (PubKeyXO, [SubscriptionId])
  , currentPost :: Maybe EventId
  , version :: Text
  , inboxModelState :: InboxModelState
  , cacheClearer :: Maybe (Async ())
  }


-- | Follow.
data Follow = Follow
  { pubkey :: PubKeyXO
  , petName :: Maybe Text
  } deriving (Eq, Show, Generic)

-- | ToJSON instance for Follow
instance ToJSON Follow where
    toJSON Follow{..} = object
        [ "pubkey" .= pubkey
        , "petName" .= petName
        ]


-- | FromJSON instance for Follow
instance FromJSON Follow where
    parseJSON = withObject "Follow" $ \v -> Follow
        <$> v .: "pubkey"
        <*> v .: "petName"


-- | Initial application state.
initialState :: AppState
initialState = AppState
  { keyPair = Nothing
  , currentScreen = KeyMgmt
  , currentFeed = Nothing
  , currentCommentFeed = Nothing
  , currentProfile = Nothing
  , currentPost = Nothing
  , version = pack runtimeVersion
  , inboxModelState = Stopped
  , cacheClearer = Nothing
  }
