{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, (.:), (.=), withObject, object)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text, pack)
import Control.Concurrent.Async (Async)
import GHC.Generics (Generic)
import Nostr.Event (Event, EventId)
import Nostr.Keys (KeyPair, PubKeyXO)
import Nostr.Types (Relay, RelayURI, SubscriptionId)
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


-- | Application state.
data AppState = AppState
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen -- @todo remove maybe?
  , currentFeed :: Maybe FeedFilter
  , currentCommentFeed :: Maybe FeedFilter
  , currentProfile :: Maybe (PubKeyXO, [SubscriptionId])
  , currentPost :: Maybe EventId
  , version :: Text
  , inboxModelState :: InboxModelState
  , currentDMRelays :: Map RelayURI ()
  , currentGeneralRelays :: Map RelayURI Relay
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
  , currentDMRelays = Map.empty
  , currentGeneralRelays = Map.empty
  , cacheClearer = Nothing
  }
