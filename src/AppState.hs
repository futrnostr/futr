module AppState where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful.Concurrent.STM (TChan, TQueue)
import Graphics.QML (ObjRef)

import Nostr.Keys (KeyPair, PubKeyXO)
import Nostr.Types (Event, EventId, Profile, RelayInfo, RelayURI, Request, Response, SubscriptionId)

-- | State for RelayPool handling.
data RelayPoolState = RelayPoolState
    { relays        :: Map RelayURI RelayData
    }

-- | Data for each relay.
data RelayData = RelayData
  { connected      :: Bool
  , relayInfo      :: RelayInfo
  , requestChannel :: TChan Request
  , responseQueue  :: TQueue Response
  , notices        :: [Text]
  , subscriptions  :: [SubscriptionId]
  , retryCount     :: Int
  }

-- | Initial state for RelayPool.
initialRelayPoolState :: RelayPoolState
initialRelayPoolState = RelayPoolState
  { relays = Map.empty
  }

data AppScreen
    = KeyMgmt
    | Home
    deriving (Eq, Read, Show)

data ChatMessage = ChatMessage
  { chatMessageId :: EventId
  , chatMessage :: Text
  , author :: PubKeyXO
  , timestamp :: Text
  , seenOn :: [RelayURI]
  }

data EventConfirmation = EventConfirmation
  { relay :: RelayURI
  , waitingForConfirmation :: Bool
  , accepted :: Bool
  , message :: Text
  }

data AppState = AppState
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen
  , events :: Map EventId (Event, [RelayURI])
  , chats :: Map PubKeyXO [ChatMessage]
  , profiles :: Map PubKeyXO (Profile, Int)
  , follows :: FollowModel
  , confirmations :: Map EventId [EventConfirmation]
  , currentChatRecipient :: Maybe PubKeyXO
  }

data FollowModel = FollowModel
  { followList :: Map PubKeyXO [Follow]
  , objRef :: Maybe (ObjRef ())
  }

data Follow = Follow
  { pubkey :: PubKeyXO
  , relayURI :: Maybe RelayURI
  , petName :: Maybe Text
  }

initialState :: AppState
initialState = AppState
  { keyPair = Nothing
  , currentScreen = KeyMgmt
  , events = Map.empty
  , chats = Map.empty
  , profiles = Map.empty
  , follows = FollowModel Map.empty Nothing
  , confirmations = Map.empty
  , currentChatRecipient = Nothing
  }
