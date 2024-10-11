-- | Module: Nostr.Types
-- Defines types related to the Nostr protocol.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Types where

import Basement.IntegralConv (wordToInt)
import Control.Applicative ((<|>))
import Control.Lens ((^.), (^?), (<&>), _Right)
import Control.Monad (mzero)
import Data.Aeson hiding (Error)
import Data.Aeson.Encoding (list, text)
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Default
import Data.Foldable (toList)
import Data.Function (on)
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text, isInfixOf, toLower)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Text.URI (URI, mkURI, render, unRText)
import Text.URI.Lens (uriAuthority, uriPath, uriScheme, authHost, authPort)
import qualified Text.URI.QQ as QQ

import Nostr.Keys (PubKeyXO(..), Signature, byteStringToHex, exportPubKeyXO, exportSignature)

-- | Represents a wrapped URI used within a relay.
newtype RelayURI = RelayURI URI deriving (Eq, Generic, Ord, Show)


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
data Filter
  = MetadataFilter [PubKeyXO]
  | FollowListFilter [PubKeyXO]
  | ShortTextNoteFilter [PubKeyXO] Int
  | LinkedEvents [EventId] Int
  | AllNotes Int
  | AllMetadata Int
  deriving (Eq, Generic,Show)


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
  | DirectMessage   -- NIP-17
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
  | UnknownTag Value  -- Store the original JSON data as an Aeson Value
  deriving (Eq, Generic, Show)


-- | Represents an event.
data Event = Event
  { eventId    :: EventId
  , pubKey     :: PubKeyXO
  , createdAt :: Int
  , kind       :: Kind
  , tags       :: [Tag]
  , content    :: Text
  , sig        :: Signature
  }
  deriving (Eq, Generic, Show)


-- | Represents an unsigned event.
data UnsignedEvent = UnsignedEvent
  { pubKey'     :: PubKeyXO
  , createdAt' :: Int
  , kind'       :: Kind
  , tags'       :: [Tag]
  , content'    :: Text
  }
  deriving (Eq, Generic, Show)


-- | Represents a received event with its associated relays.
type ReceivedEvent = (Event, [Relay])


-- | Represents a contact with a public key and an optional display name.
type Contact = (PubKeyXO, Maybe DisplayName)

type Name = Text
type DisplayName = Text
type About = Text
type Picture = Text
type Banner = Text
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
  | "duplicate:"    `isInfixOf` errMsg = Duplicate
  | "pow:"          `isInfixOf` errMsg = Pow
  | "blocked:"      `isInfixOf` errMsg = Blocked
  | "rate-limited:" `isInfixOf` errMsg = RateLimited
  | "invalid:"      `isInfixOf` errMsg = Invalid
  | otherwise                          = Error


-- | Decodes a hex string to a byte string.
decodeHex :: ConvertibleStrings a ByteString => a -> Maybe ByteString
decodeHex str =
  case B16.decode $ cs str of
    Right bs -> Just bs
    Left _   -> Nothing

-- Instance declarations

instance Show EventId where
  showsPrec _ = shows . B16.encode . getEventId

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

instance ToJSON EventId where
  toJSON = String . decodeUtf8 . B16.encode . getEventId
  toEncoding = text . decodeUtf8 . B16.encode . getEventId

instance FromJSON Event where
  parseJSON = withObject "event data" $ \e -> Event
    <$> e .: "id"
    <*> e .: "pubkey"
    <*> e .: "created_at"
    <*> e .: "kind"
    <*> e .: "tags"
    <*> e .: "content"
    <*> e .: "sig"

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

instance ToJSON UnsignedEvent where
  toEncoding UnsignedEvent {..} = list id
     [ toEncoding (0 :: Int)
     , text $ byteStringToHex $ exportPubKeyXO pubKey'
     , toEncoding createdAt'
     , toEncoding kind'
     , toEncoding tags'
     , text content'
     ]

instance FromJSON Tag where
  parseJSON v@(Array arr) =
    case V.toList arr of
      ("e":rest) -> either (const $ return $ UnknownTag v) return $ parseEither (parseETag rest) v
      ("p":rest) -> either (const $ return $ UnknownTag v) return $ parseEither (parsePTag rest) v
      _          -> return $ UnknownTag v
  parseJSON v = return $ UnknownTag v

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

parsePTag :: [Value] -> Value -> Parser Tag
parsePTag rest _ = do
  case rest of
    (pubKeyVal:relayVal:nameVal:_) -> do
      pubKey <- parseJSONSafe pubKeyVal
      relay <- parseMaybeRelayURI relayVal
      name <- parseMaybeDisplayName nameVal
      return $ PTag pubKey relay name
    (pubKeyVal:relayVal:_) -> do
      pubKey <- parseJSONSafe pubKeyVal
      relay <- parseMaybeRelayURI relayVal
      return $ PTag pubKey relay Nothing
    (pubKeyVal:_) -> do
      pubKey <- parseJSONSafe pubKeyVal
      return $ PTag pubKey Nothing Nothing
    _ -> fail "Invalid PTag format"

parseJSONSafe :: FromJSON a => Value -> Parser a
parseJSONSafe v = case parseEither parseJSON v of
  Left _ -> fail "Parsing failed"
  Right x -> return x

parseMaybeRelayURI :: Value -> Parser (Maybe RelayURI)
parseMaybeRelayURI Null = return Nothing
parseMaybeRelayURI v = (Just <$> parseJSONSafe v) <|> return Nothing

parseMaybeRelationship :: Value -> Parser (Maybe Relationship)
parseMaybeRelationship Null = return Nothing
parseMaybeRelationship v = (Just <$> parseJSONSafe v) <|> return Nothing

parseMaybeDisplayName :: Value -> Parser (Maybe DisplayName)
parseMaybeDisplayName Null = return Nothing
parseMaybeDisplayName v = (Just <$> parseJSONSafe v) <|> return Nothing

instance ToJSON Tag where
  toEncoding (ETag eventId Nothing Nothing) =
    list id [text "e", text $ decodeUtf8 $ B16.encode $ getEventId eventId]
  toEncoding (ETag eventId relayURL Nothing) =
    list id
      [ text "e"
      , text $ decodeUtf8 $ B16.encode $ getEventId eventId
      , maybe (text "") (text . render . unwrapRelayURI) relayURL
      ]
  toEncoding (ETag eventId relayURL marker) =
    list id
      [ text "e"
      , text $ decodeUtf8 $ B16.encode $ getEventId eventId
      , maybe (text "") (text . render . unwrapRelayURI) relayURL
      , case marker of
          Just Reply -> text "reply"
          Just Root -> text "root"
      ]
  toEncoding (PTag xo relayURL name) =
    list id
      [ text "p"
      , toEncoding xo
      , maybe (text "") (text . render . unwrapRelayURI) relayURL
      , maybe (text "") text name
      ]
  toEncoding (UnknownTag v) =
    list id [toEncoding v]


instance FromJSON Relationship where
 parseJSON = withText "Relationship" $ \m -> do
   case toLower m of
     "reply" -> return Reply
     "root"  -> return Root
     _       -> mzero


instance ToJSON Relationship where
  toEncoding Reply = text "reply"
  toEncoding Root = text "root"


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

instance ToJSON Subscription where
  toEncoding (Subscription efs s) = pairs $ "subId" .= s <> "filters" .= efs

instance ToJSON Request where
  toEncoding req = case req of
    SendEvent event -> list id [text "EVENT", toEncoding event]
    Subscribe (Subscription filters subId) -> list id $ text "REQ" : text subId : map toEncoding (toList filters)
    Close subId -> list text ["CLOSE", subId]
    Disconnect -> list text ["DISCONNECT"]

-- | Converts a `RelayURI` into its JSON representation.
instance FromJSON RelayURI where
  parseJSON = withText "URI" $ \u ->
    case mkURI u of
      Just u' -> return $ RelayURI u'
      Nothing -> fail $ "Invalid relay URI: " ++ show u

-- | Parses a JSON value into a `RelayURI`.
instance ToJSON RelayURI where
  toJSON (RelayURI u) = toJSON $ render u
  toEncoding (RelayURI u) = toEncoding $ render u

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
    "uri" .= render (unwrapRelayURI $ uri r) <>
    "info" .= info r

-- | 'FromJSON' instance for 'Kind'.
-- This allows parsing JSON numbers into 'Kind' values.
instance FromJSON Kind where
  parseJSON = withScientific "kind" $ \k -> case k of
    0  -> return Metadata
    1  -> return ShortTextNote
    3  -> return FollowList
    5  -> return EventDeletion
    6  -> return Repost
    7  -> return Reaction
    13 -> return Seal
    14 -> return DirectMessage
    _  -> mzero

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
  toEncoding DirectMessage = toEncoding (14 :: Int)

instance ToJSON Filter where
  toEncoding (MetadataFilter xos) = pairs $
    "authors" .= xos <>
    "kinds" .= [Metadata] <>
    "limit" .= (500 :: Int)
  toEncoding (FollowListFilter xos) = pairs $
    "authors" .= xos <>
    "kinds" .= [FollowList] <>
    "limit" .= (500 :: Int)
  toEncoding (ShortTextNoteFilter xos now) = pairs $
    "authors" .= xos <>
    "kinds" .= [ShortTextNote, EventDeletion] <>
    "until" .= (fromIntegral (now + 60) :: Integer) <>
    "limit" .= (500 :: Int)
  toEncoding (LinkedEvents eids now) = pairs $
    "kinds" .= [ShortTextNote] <>
    "#e" .= eids <>
    "until" .= (fromIntegral (now + 60) :: Integer) <>
    "limit" .= (500 :: Int)
  toEncoding (AllNotes now) = pairs $
    "kinds" .= [ShortTextNote] <>
    "until" .= (fromIntegral (now + 60) :: Integer) <>
    "limit" .= (500 :: Int)
  toEncoding (AllMetadata now) = pairs $
    "kinds" .= [Metadata] <>
    "until" .= (fromIntegral (now + 60) :: Integer) <>
    "limit" .= (500 :: Int)

instance Default Profile where
  def = Profile Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON Profile where
  toEncoding (Profile name displayName about picture nip05 banner) = pairs $
    "name" .= name <>
    "display_name" .= displayName <>
    "about" .= about <>
    "picture" .= picture <>
    "nip05" .= nip05 <>
    "banner" .= banner

instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .: "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
    <*> e .:? "nip05"
    <*> e .:? "banner"

-- Relay Helper functions

-- | Provides a default list of relays.
defaultRelays :: [Relay]
defaultRelays =
  [ Relay (RelayURI [QQ.uri|wss://nos.lol|]) (RelayInfo True True)
  --, Relay (RelayURI [QQ.uri|ws://127.0.0.1:7777|]) (RelayInfo True True)
  , Relay (RelayURI [QQ.uri|wss://relay.damus.io|]) (RelayInfo True True)
  ]

-- | Unwrap relay URI.
unwrapRelayURI :: RelayURI -> URI
unwrapRelayURI (RelayURI u) = u

-- | Retrieves the textual representation of the relay's URI.
relayName :: Relay -> Text
relayName r = render $ unwrapRelayURI $ uri r

relayURIToText :: RelayURI -> Text
relayURIToText (RelayURI u) = render u

-- | Extracts the scheme of a relay's URI.
extractScheme :: Relay -> Maybe Text
extractScheme r = uri' ^. uriScheme <&> unRText
  where
    uri' = unwrapRelayURI $ uri r

-- | Extracts the hostname of a relay's URI.
extractHostname :: Relay -> Maybe Text
extractHostname r = uri' ^? uriAuthority . _Right . authHost <&> unRText
  where
    uri' = unwrapRelayURI $ uri r

-- | Extracts the port of a relay's URI.
extractPort :: Relay -> Int
extractPort r =
  case uri' ^? uriAuthority . _Right . authPort of
    Just (Just p) -> wordToInt p
    _ -> if extractScheme r == Just "wss" then 443 else 80
  where
    uri' = unwrapRelayURI $ uri r

-- | Extracts the path of a relay's URI.
extractPath :: Relay -> Text
extractPath r =
  case uri' ^? uriPath of
    Just [] -> "/"
    Just p  -> foldMap ("/" <>) (map unRText p)
    _       -> "/"
  where
    uri' = unwrapRelayURI $ uri r

-- | Checks if two relays are the same based on URI.
sameRelay :: Relay -> Relay -> Bool
sameRelay = (==) `on` uri
