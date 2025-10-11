-- | Module: Nostr.Types
-- Defines types related to the Nostr protocol.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Types where

import Control.Monad (when)
import Data.Aeson hiding (Error)
import Data.Aeson.Encoding (list, text, pair)
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser)
import Data.ByteString.Base16 qualified as B16
import Data.Function (on)
import Data.List (dropWhileEnd)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Prelude hiding (until)
import Network.URI (URI(..), URIAuth(..), parseURI, uriAuthority, uriRegName, uriScheme)
import Text.Read (readMaybe)

import Nostr.Event (Event, EventId(..), Kind(..), decodeHex)
import Nostr.Keys (PubKeyXO(..), byteStringToHex, exportPubKeyXO)


-- | Represents a subscription id as text.
type SubscriptionId = Text


-- | Represents a relay URI.
type RelayURI = Text


-- | Relay descriptor
data Relay
  = InboxRelay RelayURI        -- Read-only relay
  | OutboxRelay RelayURI       -- Write-only relay
  | InboxOutboxRelay RelayURI  -- Both read and write (also for DM)
  deriving (Eq, Generic, Show)


instance Ord Relay where
  compare r r' = compare (getUri r) (getUri r')


instance ToJSON Relay where
  toJSON (InboxRelay uri) = object ["uri" .= uri, "read" .= True, "write" .= False]
  toJSON (OutboxRelay uri) = object ["uri" .= uri, "read" .= False, "write" .= True]
  toJSON (InboxOutboxRelay uri) = object ["uri" .= uri, "read" .= True, "write" .= True]


instance FromJSON Relay where
  parseJSON = withObject "Relay" $ \v -> do
    uri <- v .: "uri"
    read' <- v .: "read"
    write <- v .: "write"
    pure $ case (read', write) of
      (True, False) -> InboxRelay uri
      (False, True) -> OutboxRelay uri
      (True, True) -> InboxOutboxRelay uri
      _ -> InboxOutboxRelay uri -- Default case


-- | URI validation for relays (scheme ws/wss with authority)
isValidRelayURI :: RelayURI -> Bool
isValidRelayURI uriText =
  case parseURI (T.unpack uriText) of
    Just uri ->
      let scheme = uriScheme uri
          authority = uriAuthority uri
      in (scheme == "ws:" || scheme == "wss:") && maybe False (not . null . uriRegName) authority
    Nothing -> False


-- | Normalize a relay URI according to RFC 3986
normalizeRelayURI :: RelayURI -> RelayURI
normalizeRelayURI uri = case parseURI (T.unpack uri) of
    Just uri' -> T.pack $
        (if uriScheme uri' == "wss:" then "wss://" else "ws://") ++
        maybe "" (\auth ->
            let hostPort = uriRegName auth ++
                    case uriPort auth of
                        ":80" | uriScheme uri' == "ws:" -> ""
                        ":443" | uriScheme uri' == "wss:" -> ""
                        p -> p
            in hostPort
        ) (uriAuthority uri') ++
        dropWhileEnd (== '/') (uriPath uri' ++ uriQuery uri' ++ uriFragment uri')
    Nothing -> uri


-- | Get the URI from a Relay
getUri :: Relay -> RelayURI
getUri (InboxRelay uri)        = uri
getUri (OutboxRelay uri)       = uri
getUri (InboxOutboxRelay uri)  = uri


-- | Check if a relay is inbox capable
isInboxCapable :: Relay -> Bool
isInboxCapable (InboxRelay _) = True
isInboxCapable (InboxOutboxRelay _) = True
isInboxCapable (OutboxRelay _) = False


-- | Check if a relay is outbox capable
isOutboxCapable :: Relay -> Bool
isOutboxCapable (OutboxRelay _) = True
isOutboxCapable (InboxOutboxRelay _) = True
isOutboxCapable (InboxRelay _) = False


-- | Extracts the scheme of a relay's URI.
extractScheme :: RelayURI -> Maybe Text
extractScheme u =
  case T.splitOn "://" u of
    (scheme:_) -> Just scheme
    _ -> Nothing


-- | Extracts the hostname of a relay's URI.
extractHostname :: RelayURI -> Maybe Text
extractHostname u =
  case T.splitOn "://" u of
    (_:rest:_) -> Just $ T.takeWhile (/= ':') $ T.dropWhile (== '/') rest
    _ -> Nothing


-- | Extracts the port of a relay's URI.
extractPort :: RelayURI -> Int
extractPort u =
  case T.splitOn ":" $ T.dropWhile (/= ':') $ T.dropWhile (/= '/') $ T.dropWhile (/= ':') u of
    (_:portStr:_) -> maybe (defaultPort scheme) id $ readMaybe $ T.unpack portStr
    _ -> defaultPort scheme
  where
    scheme = extractScheme u
    defaultPort (Just "wss") = 443
    defaultPort (Just "ws") = 80
    defaultPort _ = 443


-- | Extracts the path of a relay's URI.
extractPath :: RelayURI -> Text
extractPath u =
  case T.splitOn "://" u of
    (_:rest:_) ->
      let withoutHost = T.dropWhile (/= '/') rest
      in if T.null withoutHost then "/" else withoutHost
    _ -> "/"


-- | Checks if two relays are the same based on URI.
sameRelay :: Relay -> Relay -> Bool
sameRelay = (==) `on` getUri


-- | Default relays
defaultGeneralRelays :: ([Relay], Int)
defaultGeneralRelays =
  ( [ InboxOutboxRelay "wss://nos.lol"
    , InboxOutboxRelay "wss://nostr.bitcoiner.social"
    , InboxOutboxRelay "wss://relay.nostr.bg"
    , InboxOutboxRelay "wss://nostr.mom"
    , InboxOutboxRelay "wss://offchain.pub"
    , InboxRelay "wss://relay.damus.io"
    ],
    0
  )


-- | Default DM relays
defaultDMRelays :: ([RelayURI], Int)
defaultDMRelays =
  ( [ "wss://auth.nostr1.com"
    , "wss://inbox.azzamo.net"
    ], 0 )


-- | Represents a subscription.
data Subscription = Subscription
  { subId   :: SubscriptionId
  , filter :: Filter
  }
  deriving (Eq, Generic, Show)


-- | Represents a filter for events.
data Filter = Filter
  { ids     :: Maybe [EventId]
  , authors :: Maybe [PubKeyXO]
  , kinds   :: Maybe [Kind]
  , since   :: Maybe Int
  , until   :: Maybe Int
  , limit   :: Maybe Int
  , fTags   :: Maybe (Map.Map Char [Text])
  }
  deriving (Eq, Generic, Show)


-- | Empty filter.
emptyFilter :: Filter
emptyFilter = Filter
  { ids = Nothing
  , authors = Nothing
  , kinds = Nothing
  , since = Nothing
  , until = Nothing
  , limit = Nothing
  , fTags = Nothing
  }


-- | Create a filter to track changes in the relay network topology of contacts.
topologyFilter :: [PubKeyXO] -> Filter
topologyFilter pks = emptyFilter
    { authors = Just pks
    , kinds = Just [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
    }


-- | Creates filter for gift wrapped messages.
giftWrapFilter :: PubKeyXO -> Filter
giftWrapFilter xo = emptyFilter
    { kinds = Just [GiftWrap]
    , fTags = Just $ Map.fromList [('p', [byteStringToHex $ exportPubKeyXO xo])]
    }


-- | Filter for mentions of a specific public key.
mentionsFilter :: PubKeyXO -> Filter
mentionsFilter pk = emptyFilter
    { kinds = Just [ShortTextNote, Repost, Comment, EventDeletion]
    , fTags = Just $ Map.singleton 'p' [byteStringToHex $ exportPubKeyXO pk]
    }


-- | Creates a filter for fetching metadata.
metadataFilter :: [PubKeyXO] -> Filter
metadataFilter pks = emptyFilter
    { authors = Just pks
    , kinds = Just [Metadata]
    , limit = Just $ 50 * length pks
    }


-- | Filter for comments on a specific event.
commentsFilter :: EventId -> Filter
commentsFilter eid = emptyFilter
    { kinds = Just [ShortTextNote]
    , fTags = Just $ Map.singleton 'e' [decodeUtf8 $ B16.encode $ getEventId eid]
    , limit = Just $ 2000
    }


-- | Represents a request to the relay.
data Request
  = SendEvent Event
  | Subscribe Subscription
  | Close SubscriptionId
  | Disconnect
  | Authenticate Event
  deriving (Eq, Generic, Show)


-- | Represents a response from the relay.
data Response
  = EventReceived SubscriptionId Event
  | Ok (Maybe EventId) Bool (Maybe Text)
  | Eose SubscriptionId
  | Closed SubscriptionId Text
  | Notice Text
  | Auth Text
  deriving (Eq, Show)


-- | Represents a standard prefix for error messages.
data StandardPrefix = Duplicate | Pow | Blocked | RateLimited | Invalid | Error
    deriving (Eq, Show)

-- Helper functions


-- | Converts an error message to a standard prefix.
noticeReason :: Text -> StandardPrefix
noticeReason errMsg
  | "duplicate:"    `T.isInfixOf` errMsg = Duplicate
  | "pow:"          `T.isInfixOf` errMsg = Pow
  | "blocked:"      `T.isInfixOf` errMsg = Blocked
  | "rate-limited:" `T.isInfixOf` errMsg = RateLimited
  | "invalid:"      `T.isInfixOf` errMsg = Invalid
  | otherwise                            = Error


-- | Converts a JSON array into a 'Response'.
instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0 :: Parser Text
    case type' of
      "EVENT" -> do
        subId' <- parseJSON $ arr V.! 1
        event <- parseJSON $ arr V.! 2
        return $ EventReceived subId' event
      "OK" -> do
        idStr <- parseJSON $ arr V.! 1 :: Parser Text
        bool <- parseJSON $ arr V.! 2
        message <- if V.length arr > 3
                    then Just <$> parseJSON (arr V.! 3)
                    else pure Nothing
        -- Convert empty string to Nothing, otherwise try to parse as EventId
        let eventId = if T.null idStr
                     then Nothing
                     else Just $ EventId $ fromMaybe "" $ decodeHex $ encodeUtf8 idStr
        return $ Ok eventId bool message
      "EOSE" -> do
        when (V.length arr < 2) $
          fail "Malformed EOSE message: missing subscription ID"
        subId' <- parseJSON $ arr V.! 1
        return $ Eose subId'
      "CLOSED" -> do
        subId' <- parseJSON $ arr V.! 1
        message <- parseJSON $ arr V.! 2
        return $ Closed subId' message
      "NOTICE" -> do
        message <- parseJSON $ arr V.! 1
        return $ Notice message
      "AUTH" -> do
        challenge <- parseJSON $ arr V.! 1
        return $ Auth challenge
      _ -> fail "Unknown response type"


-- | Converts a 'Subscription' to its JSON representation.
instance ToJSON Subscription where
  toEncoding (Subscription efs s) = pairs $ "subId" .= s <> "filter" .= efs


-- | Converts a 'Request' to its JSON representation.
instance ToJSON Request where
  toEncoding req = case req of
    Authenticate event -> list id [text "AUTH", toEncoding event]
    SendEvent event -> list id [text "EVENT", toEncoding event]
    Subscribe (Subscription subId f) -> list id $ text "REQ" : text subId : [ toEncoding f ]
    Close subId -> list text ["CLOSE", subId]
    Disconnect -> list text ["DISCONNECT"]


-- | 'ToJSON' instance for 'Filter'.
instance ToJSON Filter where
  toEncoding Filter {..} = pairs $ mconcat $ catMaybes
    [ fmap (pair "ids" . toEncoding) ids
    , fmap (pair "authors" . toEncoding) authors
    , fmap (pair "kinds" . toEncoding) kinds
    , fmap (pair "since" . toEncoding) since
    , fmap (pair "until" . toEncoding) until
    , fmap (pair "limit" . toEncoding) limit
    ] ++ maybe [] (pure . encodeTags) fTags
    where
      encodeTags :: Map.Map Char [Text] -> Series
      encodeTags = Map.foldrWithKey (\k v acc -> acc <> pair (fromText ("#" <> T.singleton k)) (toEncoding v)) mempty
