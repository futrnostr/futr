{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON, ToJSON, Value(..), toJSON, parseJSON, (.:), (.=), withObject, object)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Effectful.Concurrent.STM (TChan, TQueue)
import Lmdb.Types (Database, Environment, Mode(..))
import GHC.Generics (Generic)
import Nostr.Keys (KeyPair, PubKeyXO)
import Nostr.Types (Event, EventId, Filter, Profile, Relay(..), RelayURI, Request, SubscriptionId)


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


-- | Data type to store event with its relay sources
data EventWithRelays = EventWithRelays
    { event :: Event
    , relays :: Set RelayURI
    } deriving (Show, Generic, ToJSON, FromJSON)


-- | Application state.
data AppState = AppState
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen -- @todo remove maybe?
  , currentContact :: (Maybe PubKeyXO, Maybe SubscriptionId)
  , currentProfile :: Maybe PubKeyXO
  }


-- | Follow.
data Follow = Follow
  { pubkey :: PubKeyXO
  , followRelay :: Maybe Relay
  , petName :: Maybe Text
  } deriving (Eq, Show, Generic)

-- | ToJSON instance for Follow
instance ToJSON Follow where
    toJSON Follow{..} = object
        [ "pubkey" .= pubkey
        , "followRelay" .= (case followRelay of
            Just (InboxRelay uri) -> object ["contents" .= uri, "tag" .= ("InboxRelay" :: Text)]
            Nothing -> Null)
        , "petName" .= petName
        ]


-- | FromJSON instance for Follow
instance FromJSON Follow where
    parseJSON = withObject "Follow" $ \v -> Follow
        <$> v .: "pubkey"
        <*> (v .: "followRelay" >>= parseRelay)
        <*> v .: "petName"
      where
        parseRelay Null = pure Nothing
        parseRelay (Object o) = do
            tag <- o .: "tag"
            if tag == ("InboxRelay" :: Text)
                then Just . InboxRelay <$> o .: "contents"
                else fail $ "Unknown relay tag: " ++ show tag
        parseRelay _ = fail "Invalid relay format"


-- | Initial application state.
initialState :: AppState
initialState = AppState
  { keyPair = Nothing
  , currentScreen = KeyMgmt
  , currentContact = (Nothing, Nothing)
  , currentProfile = Nothing
  }
