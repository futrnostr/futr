-- | Module: Nostr.Types
-- Defines types related to the Nostr protocol.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Nostr.Types where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson hiding (Error)
import Data.Aeson.Encoding (list, text, pair)
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Scientific (toBoundedInteger)
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Prelude hiding (until)

import Nostr.Keys (PubKeyXO(..), Signature, byteStringToHex, exportPubKeyXO, exportSignature)


-- | Represents a wrapped URI used within a relay.
newtype RelayURI = RelayURI { unRelayURI :: Text } deriving (Eq, Show, Ord)


-- | Represents the information associated with a relay.
data RelayInfo = RelayInfo
  { readable  :: Bool
  , writable  :: Bool
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


-- | Represents a relay entity containing URI, relay information, and connection status.
data Relay = Relay
  { uri       :: RelayURI
  , info      :: RelayInfo
  }
  deriving (Eq, Generic, Show)


-- | Represents a subscription id as text.
type SubscriptionId = Text


-- | Represents a subscription.
data Subscription = Subscription
  { filters :: [Filter]
  , subId   :: SubscriptionId
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


-- | Represents a request to the relay.
data Request
  = SendEvent Event
  | Subscribe Subscription
  | Close SubscriptionId
  | Disconnect
  deriving (Eq, Generic, Show)


-- | Represents a response from the relay.
data Response
  = EventReceived SubscriptionId Event
  | Ok EventId Bool Text
  | Eose SubscriptionId
  | Closed SubscriptionId Text
  | Notice Text
  deriving (Eq, Show)


-- | Represents a standard prefix for error messages.
data StandardPrefix = Duplicate | Pow | Blocked | RateLimited | Invalid | Error
    deriving (Eq, Show)


-- | The 'Kind' data type represents different kinds of events in the Nostr protocol.
data Kind
  = Metadata        -- NIP-01
  | ShortTextNote   -- NIP-01
  | FollowList      -- NIP-02
  | EventDeletion   -- NIP-09
  | Repost          -- NIP-18
  | Reaction        -- NIP-25
  | Seal            -- NIP-59
  | GiftWrap        -- NIP-59
  | DirectMessage   -- NIP-17
  | UnknownKind Int
  deriving (Eq, Generic, Read, Show)


-- | Represents an event id as a byte string.
newtype EventId = EventId { getEventId :: ByteString } deriving (Eq, Ord)


-- | Represents a relationship type.
data Relationship = Reply | Root
  deriving (Eq, Generic, Show)


-- | Represents a tag in an event.
data Tag
  = ETag EventId (Maybe RelayURI) (Maybe Relationship)
  | PTag PubKeyXO (Maybe RelayURI) (Maybe DisplayName)
  | GenericTag [Value]
  deriving (Eq, Generic, Show)


-- | Represents an event.
data Event = Event
  { eventId   :: EventId
  , pubKey    :: PubKeyXO
  , createdAt :: Int
  , kind      :: Kind
  , tags      :: [Tag]
  , content   :: Text
  , sig       :: Signature
  }
  deriving (Eq, Generic, Show)


-- | Represents an unsigned event.
data UnsignedEvent = UnsignedEvent
  { pubKey'    :: PubKeyXO
  , createdAt' :: Int
  , kind'      :: Kind
  , tags'      :: [Tag]
  , content'   :: Text
  }
  deriving (Eq, Generic, Show)


-- | Represents a rumor (unsigned event).
data Rumor = Rumor
  { rumorId        :: EventId
  , rumorPubKey    :: PubKeyXO
  , rumorCreatedAt :: Int
  , rumorKind      :: Kind
  , rumorTags      :: [Tag]
  , rumorContent   :: Text
  }
  deriving (Eq, Generic, Show)


-- | Represents a received event with its associated relays.
type ReceivedEvent = (Event, [Relay])


-- | Represents a contact with a public key and an optional display name.
type Contact = (PubKeyXO, Maybe DisplayName)


-- | Represents a name.
type Name = Text

-- | Represents a display name.
type DisplayName = Text

-- | Represents an about text.
type About = Text

-- | Represents a picture.
type Picture = Text

-- | Represents a banner.
type Banner = Text

-- | Represents a NIP-05.
type Nip05 = Text


-- | Represents a user profile.
data Profile = Profile
  { name :: Maybe Text
  , displayName :: Maybe Text
  , about :: Maybe Text
  , picture :: Maybe Text
  , nip05 :: Maybe Text
  , banner :: Maybe Text
  } deriving (Eq, Generic,Show)


-- | Empty profile.
emptyProfile :: Profile
emptyProfile = Profile Nothing Nothing Nothing Nothing Nothing Nothing

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


-- | Decodes a hex string to a byte string.
decodeHex :: ConvertibleStrings a ByteString => a -> Maybe ByteString
decodeHex str =
  case B16.decode $ cs str of
    Right bs -> Just bs
    Left _   -> Nothing

-- Instance declarations

-- | Converts an 'EventId' to its string representation.
instance Show EventId where
  showsPrec _ = shows . B16.encode . getEventId


-- | Converts a JSON string into an 'EventId'.
instance FromJSON EventId where
  parseJSON = withText "EventId" $ \i -> do
    case eventId' i of
      Just e -> return e
      _      -> fail "invalid event id"
    where
      eventId' :: Text -> Maybe EventId
      eventId' t = do
        bs <- decodeHex t
        case BS.length bs of
          32 -> Just $ EventId bs
          _  -> Nothing


-- | Converts an 'EventId' to its JSON representation.
instance ToJSON EventId where
  toJSON = String . decodeUtf8 . B16.encode . getEventId
  toEncoding = text . decodeUtf8 . B16.encode . getEventId


-- | Converts a JSON object into an 'Event'.
instance FromJSON Event where
  parseJSON = withObject "event data" $ \e -> Event
    <$> e .: "id"
    <*> e .: "pubkey"
    <*> e .: "created_at"
    <*> e .: "kind"
    <*> e .: "tags"
    <*> e .: "content"
    <*> e .: "sig"


-- | Converts an 'Event' to its JSON representation.
instance ToJSON Event where
  toEncoding Event {..} = pairs
     ( "id"         .= (byteStringToHex $ getEventId eventId)
    <> "pubkey"     .= (byteStringToHex $ exportPubKeyXO pubKey)
    <> "created_at" .= createdAt
    <> "kind"       .= kind
    <> "tags"       .= tags
    <> "content"    .= content
    <> "sig"        .= (byteStringToHex $ exportSignature sig)
     )


-- | Converts an 'UnsignedEvent' to its JSON representation.
instance ToJSON UnsignedEvent where
  toEncoding UnsignedEvent {..} = list id
     [ toEncoding (0 :: Int)
     , text $ byteStringToHex $ exportPubKeyXO pubKey'
     , toEncoding createdAt'
     , toEncoding kind'
     , toEncoding tags'
     , text content'
     ]


-- | 'FromJSON' instance for 'Rumor'.
instance FromJSON Rumor where
  parseJSON = withObject "rumor data" $ \r -> Rumor
    <$> r .: "id"
    <*> r .: "pubkey"
    <*> r .: "created_at"
    <*> r .: "kind"
    <*> r .: "tags"
    <*> r .: "content"

-- | 'ToJSON' instance for 'Rumor'.
instance ToJSON Rumor where
  toEncoding Rumor {..} = pairs
     ( "id"         .= (byteStringToHex $ getEventId rumorId)
    <> "pubkey"     .= (byteStringToHex $ exportPubKeyXO rumorPubKey)
    <> "created_at" .= rumorCreatedAt
    <> "kind"       .= rumorKind
    <> "tags"       .= rumorTags
    <> "content"    .= rumorContent
     )


-- | Parses a 'Tag' from a JSON array.
instance FromJSON Tag where
  parseJSON v@(Array arr) =
    case V.toList arr of
      ("e":rest) -> either (const $ parseGenericTag v) return $ parseEither (parseETag rest) v
      ("p":rest) -> parsePTag rest v
      _          -> parseGenericTag v
  parseJSON v = parseGenericTag v


-- | Parses an ETag from a JSON array.
parseETag :: [Value] -> Value -> Parser Tag
parseETag rest _ = do
  case rest of
    [eventIdVal, relayVal, markerVal] -> do
      eventId <- parseJSONSafe eventIdVal
      relay <- parseMaybeRelayURI relayVal
      marker <- parseMaybeRelationship markerVal
      return $ ETag eventId relay marker
    [eventIdVal, relayVal] -> do
      eventId <- parseJSONSafe eventIdVal
      relay <- parseMaybeRelayURI relayVal
      return $ ETag eventId relay Nothing
    [eventIdVal] -> do
      eventId <- parseJSONSafe eventIdVal
      return $ ETag eventId Nothing Nothing
    _ -> fail "Invalid ETag format"


-- | Parses a PTag from a JSON array.
parsePTag :: [Value] -> Value -> Parser Tag
parsePTag rest _ = case rest of
    (pubkeyVal : maybeRelay : maybeName : _) -> do
      pubkey <- parseJSONSafe pubkeyVal
      relay <- parseMaybeRelayURI maybeRelay
      name <- parseMaybeDisplayName maybeName
      return $ PTag pubkey relay name
    (pubkeyVal : maybeRelay : _) -> do
      pubkey <- parseJSONSafe pubkeyVal
      relay <- parseMaybeRelayURI maybeRelay
      return $ PTag pubkey relay Nothing
    (pubkeyVal : _) -> do
      pubkey <- parseJSONSafe pubkeyVal
      return $ PTag pubkey Nothing Nothing
    _ -> fail "Invalid PTag format"


-- | Parses a JSON value safely and returns the parsed result.
parseJSONSafe :: FromJSON a => Value -> Parser a
parseJSONSafe v = case parseEither parseJSON v of
  Left _ -> fail "Parsing failed"
  Right x -> return x


-- | Parses a maybe relay URI from a JSON value.
parseMaybeRelayURI :: Value -> Parser (Maybe RelayURI)
parseMaybeRelayURI (String s) = pure (Just (RelayURI s))
parseMaybeRelayURI Null = pure Nothing
parseMaybeRelayURI _ = fail "Expected string or null for RelayURI"


-- | Parses a maybe relationship from a JSON value.
parseMaybeRelationship :: Value -> Parser (Maybe Relationship)
parseMaybeRelationship Null = return Nothing
parseMaybeRelationship v = (Just <$> parseJSONSafe v) <|> return Nothing


-- | Parses a maybe display name from a JSON value.
parseMaybeDisplayName :: Value -> Parser (Maybe DisplayName)
parseMaybeDisplayName Null = return Nothing
parseMaybeDisplayName (String t) = return (Just t)
parseMaybeDisplayName v = fail $ "Expected string for display name, got: " ++ show v


-- | Parses a generic tag from a JSON array.
parseGenericTag :: Value -> Parser Tag
parseGenericTag (Array arr) = return $ GenericTag (V.toList arr)
parseGenericTag v = fail $ "Expected array for generic tag, got: " ++ show v


-- | Converts a 'Tag' to its JSON representation.
instance ToJSON Tag where
  toEncoding (ETag eventId relayURL marker) =
    list id $
      [ text "e"
      , text $ decodeUtf8 $ B16.encode $ getEventId eventId
      ] ++
      (maybe [] (\r -> [text $ unRelayURI r]) relayURL) ++
      (case marker of
         Just Reply -> [text "reply"]
         Just Root -> [text "root"]
         Nothing -> [])
  toEncoding (PTag xo relayURL name) =
    list id $
      [ text "p"
      , toEncoding xo
      ] ++
      (maybe [] (\r -> [text $ unRelayURI r]) relayURL) ++
      (maybe [] (\n -> [text n]) name)
  toEncoding (GenericTag values) = list toEncoding values


-- | Converts a JSON string into a 'Relationship'.
instance FromJSON Relationship where
 parseJSON = withText "Relationship" $ \m -> do
   case T.toLower m of
     "reply" -> return Reply
     "root"  -> return Root
     _       -> mzero


-- | Converts a 'Relationship' to its JSON representation.
instance ToJSON Relationship where
  toEncoding Reply = text "reply"
  toEncoding Root = text "root"


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
        id' <- parseJSON $ arr V.! 1
        bool <- parseJSON $ arr V.! 2
        message <- parseJSON $ arr V.! 3
        return $ Ok id' bool message
      "EOSE" -> do
        subId' <- parseJSON $ arr V.! 1
        return $ Eose subId'
      "CLOSE" -> do
        subId' <- parseJSON $ arr V.! 1
        message <- parseJSON $ arr V.! 2
        return $ Closed subId' message
      "NOTICE" -> do
        message <- parseJSON $ arr V.! 1
        return $ Notice message
      _ -> fail "Unknown response type"


-- | Converts a 'Subscription' to its JSON representation.
instance ToJSON Subscription where
  toEncoding (Subscription efs s) = pairs $ "subId" .= s <> "filters" .= efs


-- | Converts a 'Request' to its JSON representation.
instance ToJSON Request where
  toEncoding req = case req of
    SendEvent event -> list id [text "EVENT", toEncoding event]
    Subscribe (Subscription filters subId) -> list id $ text "REQ" : text subId : map toEncoding (toList filters)
    Close subId -> list text ["CLOSE", subId]
    Disconnect -> list text ["DISCONNECT"]


-- | Converts a `RelayURI` into its JSON representation.
instance FromJSON RelayURI where
  parseJSON = withText "RelayURI" (pure . RelayURI)


-- | Parses a JSON value into a `RelayURI`.
instance ToJSON RelayURI where
  toJSON (RelayURI uri) = String uri
  toEncoding (RelayURI uri) = toEncoding uri


-- | Instance for ordering 'Relay' values based on their 'uri'.
instance Ord Relay where
  compare (Relay r _) (Relay r' _) = compare r r'


-- | Instance for parsing a 'Relay' from JSON.
instance FromJSON Relay where
  parseJSON = withObject "Relay" $ \r -> do
    uri'  <- r .: "uri"
    info' <- r .: "info"
    return $ Relay uri' info'


-- | Instance for converting a 'Relay' to JSON.
instance ToJSON Relay where
  toEncoding r = pairs $
    "uri" .= unRelayURI (uri r) <>
    "info" .= info r


-- | 'FromJSON' instance for 'Kind'.
-- This allows parsing JSON numbers into 'Kind' values.
instance FromJSON Kind where
  parseJSON = withScientific "kind" $ \k -> case toBoundedInteger k of
    Just n  -> case n of
      0  -> return Metadata
      1  -> return ShortTextNote
      3  -> return FollowList
      5  -> return EventDeletion
      6  -> return Repost
      7  -> return Reaction
      13 -> return Seal
      1059 -> return GiftWrap
      14 -> return DirectMessage
      _  -> return $ UnknownKind n
    Nothing -> fail "Expected an integer for Kind"


-- | 'ToJSON' instance for 'Kind'.
-- This allows serializing 'Kind' values into JSON numbers.
instance ToJSON Kind where
  toEncoding Metadata      = toEncoding (0 :: Int)
  toEncoding ShortTextNote = toEncoding (1 :: Int)
  toEncoding FollowList    = toEncoding (3 :: Int)
  toEncoding EventDeletion = toEncoding (5 :: Int)
  toEncoding Repost        = toEncoding (6 :: Int)
  toEncoding Reaction      = toEncoding (7 :: Int)
  toEncoding Seal          = toEncoding (13 :: Int)
  toEncoding GiftWrap      = toEncoding (1059 :: Int)
  toEncoding DirectMessage = toEncoding (14 :: Int)
  toEncoding (UnknownKind n) = toEncoding n


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


-- | 'ToJSON' instance for 'Profile'.
instance ToJSON Profile where
  toEncoding (Profile name displayName about picture nip05 banner) = pairs $
    "name" .= name <>
    "display_name" .= displayName <>
    "about" .= about <>
    "picture" .= picture <>
    "nip05" .= nip05 <>
    "banner" .= banner


-- | 'FromJSON' instance for 'Profile'.
instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .:? "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
    <*> e .:? "nip05"
    <*> e .:? "banner"

-- Relay Helper functions

-- | Provides a default list of relays.
defaultRelays :: [Relay]
defaultRelays =
  [ Relay (RelayURI "wss://nos.lol") (RelayInfo True True)
  --, Relay (RelayURI "ws://127.0.0.1:7777") (RelayInfo True True)
  , Relay (RelayURI "wss://relay.damus.io") (RelayInfo True True)
  --, Relay (RelayURI "wss://relay.0xchat.com") (RelayInfo True True)
  ]


-- | Retrieves the textual representation of the relay's URI.
relayName :: Relay -> Text
relayName r = unRelayURI $ uri r


-- | Converts a 'RelayURI' to a 'Text'.
relayURIToText :: RelayURI -> Text
relayURIToText = unRelayURI


-- | Extracts the scheme of a relay's URI.
extractScheme :: Relay -> Maybe Text
extractScheme r =
  case T.splitOn "://" u of
    (scheme:_) -> Just scheme
    _ -> Nothing
  where
    u = unRelayURI $ uri r


-- | Extracts the hostname of a relay's URI.
extractHostname :: Relay -> Maybe Text
extractHostname r =
  case T.splitOn "://" u of
    (_:rest:_) -> Just $ T.takeWhile (/= ':') $ T.dropWhile (== '/') rest
    _ -> Nothing
  where
    u = unRelayURI $ uri r


-- | Extracts the port of a relay's URI.
extractPort :: Relay -> Int
extractPort r =
  case extractScheme r of
    Just "wss" -> 443
    Just "ws" -> 80
    _ -> 80  -- Default to 80 if scheme is unknown


-- | Extracts the path of a relay's URI.
extractPath :: Relay -> Text
extractPath (Relay (RelayURI u) _) =
  case T.splitOn "://" u of
    (_:rest:_) ->
      let withoutHost = T.dropWhile (/= '/') rest
      in if T.null withoutHost then "/" else withoutHost
    _ -> "/"


-- | Checks if two relays are the same based on URI.
sameRelay :: Relay -> Relay -> Bool
sameRelay = (==) `on` uri


-- Helper functions to create specific filters

-- | Creates a filter for metadata.
metadataFilter :: [PubKeyXO] -> Filter
metadataFilter authors = Filter
  { ids = Nothing
  , authors = Just authors
  , kinds = Just [Metadata]
  , since = Nothing
  , until = Nothing
  , limit = Just 500
  , fTags = Nothing
  }


-- | Creates a filter for follow list.
followListFilter :: [PubKeyXO] -> Filter
followListFilter authors = Filter
  { ids = Nothing
  , authors = Just authors
  , kinds = Just [FollowList]
  , since = Nothing
  , until = Nothing
  , limit = Just 500
  , fTags = Nothing
  }


-- | Creates a filter for short text notes.
shortTextNoteFilter :: [PubKeyXO] -> Int -> Filter
shortTextNoteFilter authors now = Filter
  { ids = Nothing
  , authors = Just authors
  , kinds = Just [ShortTextNote, EventDeletion]
  , since = Nothing
  , until = Just (now + 60)
  , limit = Just 500
  , fTags = Nothing
  }


-- | Creates filter for gift wrapped messages.
giftWrapFilter :: PubKeyXO -> Filter
giftWrapFilter xo =
  Filter
    { ids = Nothing
    , authors = Nothing
    , kinds = Just [GiftWrap]
    , since = Nothing
    , until = Nothing
    , limit = Just 500
    , fTags = Just $ Map.fromList [('p', [byteStringToHex $ exportPubKeyXO xo])]
    }