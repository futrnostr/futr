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
  , objRef :: Maybe (ObjRef ())
  , currentScreen :: AppScreen
  , events :: Map EventId (Event, [RelayURI])
  , chats :: Map PubKeyXO [ChatMessage]
  , profiles :: Map PubKeyXO (Profile, Int)
  , follows :: Map PubKeyXO [(PubKeyXO, Maybe RelayURI, Maybe Text)]
  , confirmations :: Map EventId [EventConfirmation]
  }

initialState :: AppState
initialState = AppState
  { keyPair = Nothing
  , objRef = Nothing
  , currentScreen = KeyMgmt
  , events = Map.empty
  , chats = Map.empty
  , profiles = Map.empty
  , follows = Map.empty
  , confirmations = Map.empty
  }
