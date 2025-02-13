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
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Network.URI (URI(..), parseURI, uriAuthority, uriRegName, uriScheme)
import Prelude hiding (until)
import Text.Read (readMaybe)

import Nostr.Keys (PubKeyXO(..), Signature, byteStringToHex, exportPubKeyXO, exportSignature, importPubKeyXO)

-- | Represents a relay URI.
type RelayURI = Text


-- | Represents a relay with its URI and type combined.
data Relay
  = InboxRelay RelayURI        -- Read-only relay
  | OutboxRelay RelayURI       -- Write-only relay
  | InboxOutboxRelay RelayURI  -- Both read and write (also for DM)
  deriving (Eq, Generic, Show)


-- | Instance for ordering 'Relay' values based on their URI.
instance Ord Relay where
  compare r r' = compare (getUri r) (getUri r')


-- | Instance for converting a 'Relay' to JSON.
instance ToJSON Relay where
  toJSON relay = case relay of
    InboxRelay uri -> toJSON ["r", uri, "read"]
    OutboxRelay uri -> toJSON ["r", uri, "write"]
    InboxOutboxRelay uri -> toJSON ["r", uri]
  toEncoding relay = case relay of
    InboxRelay uri -> list id [text "r", text uri, text "read"]
    OutboxRelay uri -> list id [text "r", text uri, text "write"]
    InboxOutboxRelay uri -> list id [text "r", text uri]


-- | Instance for parsing a 'Relay' from JSON.
instance FromJSON Relay where
  parseJSON = withArray "Relay" $ \arr -> do
    case V.toList arr of
      ["r", String uri, String "read"] -> 
        return $ InboxRelay uri
      ["r", String uri, String "write"] -> 
        return $ OutboxRelay uri
      ["r", String uri] -> 
        return $ InboxOutboxRelay uri
      _ -> fail "Invalid relay format"



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


-- | Check if a relay URI is valid
isValidRelayURI :: RelayURI -> Bool
isValidRelayURI uriText =
    case parseURI (T.unpack uriText) of
        Just uri ->
            let scheme = uriScheme uri
                authority = uriAuthority uri
            in (scheme == "ws:" || scheme == "wss:") &&
                maybe False (not . null . uriRegName) authority
        Nothing -> False


-- | Represents a subscription id as text.
type SubscriptionId = Text


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
  | Ok EventId Bool Text
  | Eose SubscriptionId
  | Closed SubscriptionId Text
  | Notice Text
  | Auth Text
  deriving (Eq, Show)


-- | Represents a standard prefix for error messages.
data StandardPrefix = Duplicate | Pow | Blocked | RateLimited | Invalid | Error
    deriving (Eq, Show)


-- | The 'Kind' data type represents different kinds of events in the Nostr protocol.
data Kind
  = Metadata                -- NIP-01 (kind 0)
  | ShortTextNote           -- NIP-01 (kind 1)
  | FollowList              -- NIP-02 (kind 3)
  | EventDeletion           -- NIP-09 (kind 5)
  | Repost                  -- NIP-18 (kind 6)
  | Reaction                -- NIP-25 (kind 7)
  | GenericRepost           -- NIP-18 (kind 16)
  | Seal                    -- NIP-59 (kind 13)
  | GiftWrap                -- NIP-59 (kind 1059)
  | DirectMessage           -- NIP-17 (kind 14)
  | PreferredDMRelays       -- NIP-17 (kind 10050)
  | CanonicalAuthentication -- NIP-42 (kind 22242)
  | RelayListMetadata       -- NIP-65 (kind 10002)
  | Comment                 -- NIP-22 (kind 1111)
  | UnknownKind Int
  deriving (Eq, Generic, Read, Show)


kindToInt :: Kind -> Int
kindToInt = \case
  Metadata -> 0
  ShortTextNote -> 1
  FollowList -> 3
  EventDeletion -> 5
  Repost -> 6
  Reaction -> 7
  GenericRepost -> 16
  Seal -> 13
  GiftWrap -> 1059
  DirectMessage -> 14
  PreferredDMRelays -> 10050
  CanonicalAuthentication -> 22242
  RelayListMetadata -> 10002
  Comment -> 1111
  UnknownKind n -> n

instance Ord Kind where
    compare k1 k2 = compare (kindToInt k1) (kindToInt k2)

-- | Represents an event id as a byte string.
newtype EventId = EventId { getEventId :: ByteString } deriving (Eq, Ord)


-- | Represents a marker type.
data Marker = Reply | Root | Mention
  deriving (Eq, Generic, Show)


-- | Represents different types of external content IDs as specified in NIP-73
data ExternalContentId
  = UrlId Text              -- ^ Normalized URL without fragment
  | HashtagId Text          -- ^ Lowercase hashtag
  | GeohashId Text          -- ^ Lowercase geohash
  | IsbnId Text             -- ^ ISBN without hyphens
  | PodcastGuidId Text      -- ^ Podcast GUID
  | PodcastItemGuidId Text  -- ^ Podcast Episode GUID
  | PodcastPublisherGuidId Text -- ^ Podcast Publisher GUID
  | IsanId Text             -- ^ ISAN without version part
  | DoiId Text              -- ^ Lowercase DOI
  deriving (Eq, Generic, Show)


-- | Represents a tag in an event.
data Tag
  = ETag EventId (Maybe RelayURI) (Maybe Marker) (Maybe PubKeyXO) -- For lowercase "e"
  | ETag' EventId (Maybe RelayURI) (Maybe PubKeyXO)               -- For uppercase "E"
  | ETag'' EventId (Maybe RelayURI) (Maybe PubKeyXO)              -- For lowercase "e" without marker
  | PTag PubKeyXO (Maybe RelayURI) (Maybe DisplayName)            -- For lowercase "p"
  | PTag' PubKeyXO (Maybe RelayURI)                               -- For uppercase "P"
  | PListTag [PubKeyXO]
  | QTag EventId (Maybe RelayURI) (Maybe PubKeyXO)
  | KTag Text                                                     -- For lowercase "k" (parent kind)
  | KTag' Text                                                    -- For uppercase "K" (root kind)
  | RTag Relay
  | RelayTag RelayURI
  | ChallengeTag Text
  | ITag ExternalContentId (Maybe Text)
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


-- | Reads an 'EventId' from its string representation.
instance Read EventId where
  readsPrec _ str = case decodeHex str of
    Just bs | BS.length bs == 32 -> [(EventId bs, "")]
    _ -> []


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
  toJSON Event {..} = object
    [ "id"         .= byteStringToHex (getEventId eventId)
    , "pubkey"     .= byteStringToHex (exportPubKeyXO pubKey)
    , "created_at" .= createdAt
    , "kind"       .= kind
    , "tags"       .= tags
    , "content"    .= content
    , "sig"        .= byteStringToHex (exportSignature sig)
    ]
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
  toJSON UnsignedEvent {..} = toJSON
    [ toJSON (0 :: Int)
    , toJSON $ byteStringToHex $ exportPubKeyXO pubKey'
    , toJSON createdAt'
    , toJSON kind'
    , toJSON tags'
    , toJSON content'
    ]
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
  toJSON Rumor {..} = object
    [ "id"         .= byteStringToHex (getEventId rumorId)
    , "pubkey"     .= byteStringToHex (exportPubKeyXO rumorPubKey)
    , "created_at" .= rumorCreatedAt
    , "kind"       .= rumorKind
    , "tags"       .= rumorTags
    , "content"    .= rumorContent
    ]
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
      ("p":rest) -> either (const $ parseGenericTag v) return $ parseEither (parsePTag rest) v
      ("q":rest) -> either (const $ parseGenericTag v) return $ parseEither (parseQTag rest) v
      ("i":rest) -> either (const $ parseGenericTag v) return $ parseEither (parseITag rest) v
      ("k":rest) -> either (const $ parseGenericTag v) return $ parseEither (parseKTag rest) v
      ("r":rest) -> either (const $ parseGenericTag v) return $ parseEither (parseRTag rest) v
      ("relay":rest) -> either (const $ parseGenericTag v) return $ parseEither (parseRelayTag rest) v
      ("challenge":rest) -> either (const $ parseGenericTag v) return $ parseEither (parseChallengeTag rest) v
      _          -> parseGenericTag v
  parseJSON v = parseGenericTag v


-- | Parses an ETag from a JSON array.
parseETag :: [Value] -> Value -> Parser Tag
parseETag rest v@(Array arr) = case V.head arr of
    String "e" ->
        (case rest of
            -- Try parsing as ETag with marker first
            (eventIdVal:relayVal:markerVal:pubkeyVal:_) -> do
                eventId <- parseJSONSafe eventIdVal
                relay <- parseMaybeRelayURI relayVal
                marker <- parseMaybeMarker markerVal
                pubkey <- parseMaybePubKey pubkeyVal
                return $ ETag eventId relay marker pubkey

            -- If no marker present, parse as ETag''
            (eventIdVal:relayVal:pubkeyVal:_) -> do
                eventId <- parseJSONSafe eventIdVal
                relay <- parseMaybeRelayURI relayVal
                pubkey <- parseMaybePubKey pubkeyVal
                return $ ETag'' eventId relay pubkey

            (eventIdVal:_) -> do
                eventId <- parseJSONSafe eventIdVal
                return $ ETag'' eventId Nothing Nothing
        ) <|> parseGenericTag v

    String "E" ->
        (case rest of
            (eventIdVal:relayVal:pubkeyVal:_) -> do
                eventId <- parseJSONSafe eventIdVal
                relay <- parseMaybeRelayURI relayVal
                pubkey <- parseMaybePubKey pubkeyVal
                return $ ETag' eventId relay pubkey

            (eventIdVal:_) -> do
                eventId <- parseJSONSafe eventIdVal
                return $ ETag' eventId Nothing Nothing
        ) <|> parseGenericTag v

    _ -> fail "Invalid ETag type"


-- | Parses a PTag from a JSON array.
parsePTag :: [Value] -> Value -> Parser Tag
parsePTag rest v@(Array arr) = case V.head arr of
    -- Case 1: PListTag - multiple pubkeys in a list
    String "p" | length rest > 1 -> do
        pubkeys <- mapM parseJSONSafe rest
        return $ PListTag pubkeys
        
    -- Case 2: PTag - lowercase "p" with single pubkey
    String "p" -> case rest of
        [pubkeyVal, relayVal, nameVal] -> do
            pubkey <- parseJSONSafe pubkeyVal
            relay <- parseMaybeRelayURI relayVal
            name <- parseMaybeDisplayName nameVal
            return $ PTag pubkey relay name
        [pubkeyVal, relayVal] -> do
            pubkey <- parseJSONSafe pubkeyVal
            relay <- parseMaybeRelayURI relayVal
            return $ PTag pubkey relay Nothing
        [pubkeyVal] -> do
            pubkey <- parseJSONSafe pubkeyVal
            return $ PTag pubkey Nothing Nothing
        _ -> parseGenericTag v
            
    -- Case 3: PTag' - uppercase "P" with single pubkey
    String "P" -> case rest of
        [pubkeyVal, relayVal] -> do
            pubkey <- parseJSONSafe pubkeyVal
            relay <- parseMaybeRelayURI relayVal
            return $ PTag' pubkey relay
        [pubkeyVal] -> do
            pubkey <- parseJSONSafe pubkeyVal
            return $ PTag' pubkey Nothing
        _ -> parseGenericTag v
            
    _ -> parseGenericTag v
parsePTag _ v = parseGenericTag v


-- | Parses a JSON value safely and returns the parsed result.
parseJSONSafe :: FromJSON a => Value -> Parser a
parseJSONSafe v = case parseEither parseJSON v of
  Left _ -> fail "Parsing failed"
  Right x -> return x


-- | Parses a maybe relay URI from a JSON value.
parseMaybeRelayURI :: Value -> Parser (Maybe RelayURI)
parseMaybeRelayURI (String s) | T.null s = pure Nothing
parseMaybeRelayURI (String s) = pure (Just s)
parseMaybeRelayURI Null = pure Nothing
parseMaybeRelayURI _ = fail "Expected string or null for RelayURI"


-- | Parses a maybe marker from a JSON value.
parseMaybeMarker :: Value -> Parser (Maybe Marker)
parseMaybeMarker Null = return Nothing
parseMaybeMarker v = (Just <$> parseJSONSafe v) <|> return Nothing


-- | Parses a maybe display name from a JSON value.
parseMaybeDisplayName :: Value -> Parser (Maybe DisplayName)
parseMaybeDisplayName Null = return Nothing
parseMaybeDisplayName (String t) = return (Just t)
parseMaybeDisplayName v = fail $ "Expected string for display name, got: " ++ show v


-- | Parses a relay tag from a JSON array.
parseRTag :: [Value] -> Value -> Parser Tag
parseRTag rest _ = case rest of
  [relayVal, markerVal] -> do
    relayURI' <- parseRelayURI relayVal
    marker <- parseJSON markerVal :: Parser Text
    case T.toLower marker of
      "write" -> return $ RTag (OutboxRelay relayURI')
      "read"  -> return $ RTag (InboxRelay relayURI')
      _ -> fail "Invalid RTag marker"
  [relayVal] -> do
    relayURI' <- parseRelayURI relayVal
    return $ RTag (InboxOutboxRelay relayURI')
  _ -> fail "Invalid RTag format"

-- | Parses a relay tag from a JSON array.
parseRelayTag :: [Value] -> Value -> Parser Tag
parseRelayTag rest _ = case rest of
  [relayVal] -> do
    relayURI' <- parseRelayURI relayVal
    return $ RelayTag relayURI'
  _ -> fail "Invalid relay tag format. Expected single relay URI."


-- | Parses a RelayURI from a JSON value.
parseRelayURI :: Value -> Parser RelayURI
parseRelayURI (String s) = return s
parseRelayURI _ = fail "Expected string for RelayURI"


-- | Parses a challenge tag from a JSON array.
parseChallengeTag :: [Value] -> Value -> Parser Tag
parseChallengeTag rest _ = case rest of
  [challengeVal] -> do
    challenge <- parseJSONSafe challengeVal
    return $ ChallengeTag challenge
  _ -> fail "Invalid ChallengeTag format"


-- | Parses a generic tag from a JSON array.
parseGenericTag :: Value -> Parser Tag
parseGenericTag (Array arr) = return $ GenericTag (V.toList arr)
parseGenericTag v = fail $ "Expected array for generic tag, got: " ++ show v


-- | Converts a 'Tag' to its JSON representation.
instance ToJSON Tag where
  toJSON tag = case tag of
    ETag eventId relayURL marker pubkey ->
      toJSON
        [ "e"
        , decodeUtf8 $ B16.encode $ getEventId eventId
        , fromMaybe "" relayURL
        , case marker of
            Just Reply -> "reply"
            Just Root -> "root"
            Just Mention -> "mention"
            Nothing -> ""
        , maybe "" (decodeUtf8 . B16.encode . exportPubKeyXO) pubkey
        ]
    ETag' eventId relayURL pubkey ->
      toJSON $
        [ "E"
        , decodeUtf8 $ B16.encode $ getEventId eventId
        ] ++
        (maybe [] (\r -> [r]) relayURL) ++
        (maybe [] (\pk -> [decodeUtf8 $ B16.encode $ exportPubKeyXO pk]) pubkey)
    ETag'' eventId relayURL pubkey ->
      toJSON $
        [ "e"
        , decodeUtf8 $ B16.encode $ getEventId eventId
        ] ++
        (maybe [] (\r -> [r]) relayURL) ++
        (maybe [] (\pk -> [decodeUtf8 $ B16.encode $ exportPubKeyXO pk]) pubkey)
    PTag xo relayURL name ->
      toJSON $
        [ "p"
        , toJSON xo
        ] ++
        (maybe [] (\r -> [toJSON r]) relayURL) ++
        (maybe [] (\n -> [toJSON n]) name)
    PTag' xo relayURL ->
      toJSON $
        [ "P"
        , toJSON xo
        ] ++
        (maybe [] (\r -> [toJSON r]) relayURL)
    PListTag pubkeys ->
      toJSON $ "p" : map toJSON pubkeys
    QTag eventId relayURL pubkey ->
      toJSON $
        [ "q"
        , decodeUtf8 $ B16.encode $ getEventId eventId
        ] ++
        (maybe [] (\r -> [r]) relayURL) ++
        (maybe [] (\pk -> [decodeUtf8 $ B16.encode $ exportPubKeyXO pk]) pubkey)
    ITag eid urlHint ->
      toJSON $
        [ "i"
        , externalContentIdToText eid
        ] ++
        maybe [] (\url -> [url]) urlHint
    KTag kind -> toJSON [ "k", kind ]
    KTag' kind -> toJSON [ "K", kind ]
    RTag relay ->
      toJSON $ case relay of
        InboxRelay uri -> ["r", uri, "read"]
        OutboxRelay uri -> ["r", uri, "write"]
        InboxOutboxRelay uri -> ["r", uri]
    RelayTag relayUri -> toJSON ["relay", relayUri]
    ChallengeTag challenge -> toJSON ["challenge", challenge]
    GenericTag values -> toJSON values
  toEncoding tag = case tag of
    ETag eventId relayURL marker pubkey ->
      list id $
        [ text "e"
        , text $ decodeUtf8 $ B16.encode $ getEventId eventId
        , text $ fromMaybe "" relayURL
        , text $ case marker of
            Just Reply -> "reply"
            Just Root -> "root"
            Just Mention -> "mention"
            Nothing -> ""
        , text $ maybe "" (decodeUtf8 . B16.encode . exportPubKeyXO) pubkey
        ]
    ETag' eventId relayURL pubkey ->
      list id $
        [ text "E"
        , text $ decodeUtf8 $ B16.encode $ getEventId eventId
        ] ++
        (maybe [] (\r -> [text r]) relayURL) ++
        (maybe [] (\pk -> [text $ decodeUtf8 $ B16.encode $ exportPubKeyXO pk]) pubkey)
    ETag'' eventId relayURL pubkey ->
      list id $
        [ text "e"
        , text $ decodeUtf8 $ B16.encode $ getEventId eventId
        ] ++
        (maybe [] (\r -> [text r]) relayURL) ++
        (maybe [] (\pk -> [text $ decodeUtf8 $ B16.encode $ exportPubKeyXO pk]) pubkey)
    PTag xo relayURL name ->
      list id $
        [ text "p"
        , toEncoding xo
        ] ++
        (maybe [] (\r -> [text r]) relayURL) ++
        (maybe [] (\n -> [text n]) name)
    PTag' xo relayURL ->
      list id $
        [ text "P"
        , toEncoding xo
        ] ++
        (maybe [] (\r -> [text r]) relayURL)
    PListTag pubkeys ->
      list id $ text "p" : map toEncoding pubkeys
    QTag eventId relayURL pubkey ->
      list id $
        [ text "q"
        , text $ decodeUtf8 $ B16.encode $ getEventId eventId
        ] ++
        (maybe [] (\r -> [text r]) relayURL) ++
        (maybe [] (\pk -> [text $ decodeUtf8 $ B16.encode $ exportPubKeyXO pk]) pubkey)
    ITag eid urlHint ->
      list id $
        [ text "i"
        , text (externalContentIdToText eid)
        ] ++
        maybe [] (\url -> [text url]) urlHint
    KTag kind -> list id [ text "k", text kind ]
    KTag' kind -> list id [ text "K", text kind ]
    RTag relay ->
      list id $ case relay of
        InboxRelay uri -> [text "r", text uri, text "read"]
        OutboxRelay uri -> [text "r", text uri, text "write"]
        InboxOutboxRelay uri -> [text "r", text uri]
    RelayTag relayUri -> list id $ [text "relay", text relayUri]
    ChallengeTag challenge -> list id [text "challenge", text challenge]
    GenericTag values -> list toEncoding values


-- | Converts a JSON string into a 'Marker'.
instance FromJSON Marker where
 parseJSON = withText "Marker" $ \m -> do
   case T.toLower m of
     "reply" -> return Reply
     "root"  -> return Root
     "mention" -> return Mention
     _       -> mzero


-- | Converts a 'Marker' to its JSON representation.
instance ToJSON Marker where
  toEncoding Reply = text "reply"
  toEncoding Root = text "root"
  toEncoding Mention = text "mention"

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
        id' <- parseJSON $ arr V.! 1 :: Parser EventId
        bool <- parseJSON $ arr V.! 2
        message <- parseJSON $ arr V.! 3
        return $ Ok id' bool message
      "EOSE" -> do
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
      16 -> return GenericRepost
      13 -> return Seal
      1059 -> return GiftWrap
      14 -> return DirectMessage
      10050 -> return PreferredDMRelays
      22242 -> return CanonicalAuthentication
      10002 -> return RelayListMetadata
      1111 -> return Comment
      _  -> return $ UnknownKind n
    Nothing -> fail "Expected an integer for Kind"


-- | 'ToJSON' instance for 'Kind'.
-- This allows serializing 'Kind' values into JSON numbers.
instance ToJSON Kind where
  toJSON = toJSON . kindToInt
  toEncoding = toEncoding . kindToInt


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

-- | Provides a default list of general relays.
defaultGeneralRelays :: ([Relay], Int)
defaultGeneralRelays =
  ( [ InboxOutboxRelay "wss://nos.lol"
    --, InboxOutboxRelay "wss://relay.nostr.bg"
    , InboxOutboxRelay "wss://nostr.mom"
    , InboxOutboxRelay "wss://offchain.pub"
    , InboxRelay "wss://relay.damus.io"
    ],
    0
  )


-- | Provides a default list of DM relays.
defaultDMRelays :: ([RelayURI], Int)
defaultDMRelays =
  ( [ "wss://auth.nostr1.com" ], 0 )


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


-- Add parser for QTag
parseQTag :: [Value] -> Value -> Parser Tag
parseQTag rest v = case rest of
    [eventIdVal, relayVal, pubkeyVal] -> do
      eventId <- parseJSONSafe eventIdVal
      relay <- parseMaybeRelayURI relayVal
      pubkey <- parseMaybePubKey pubkeyVal
      return $ QTag eventId relay pubkey
    [eventIdVal, relayVal] -> do
      eventId <- parseJSONSafe eventIdVal
      relay <- parseMaybeRelayURI relayVal
      return $ QTag eventId relay Nothing
    [eventIdVal] -> do
      eventId <- parseJSONSafe eventIdVal
      return $ QTag eventId Nothing Nothing
    _ -> parseGenericTag v  -- Fall back to generic tag for any other format

-- Add parser for KTag
parseKTag :: [Value] -> Value -> Parser Tag
parseKTag rest v = case (v, rest) of
    (Array arr, [String kind]) -> case V.head arr of
      String "k" -> return $ KTag kind
      String "K" -> return $ KTag' kind
      _ -> fail "Invalid KTag type - must be 'K' or 'k'"
    _ -> fail "Invalid KTag format"


-- Add parser for ITag
parseITag :: [Value] -> Value -> Parser Tag
parseITag rest _ = case rest of
    [String identifier, String urlHint] -> do
      case parseExternalContentId identifier of
        Just eid -> return $ ITag eid (Just urlHint)
        Nothing -> fail $ "Invalid external identifier: " <> T.unpack identifier
    [String identifier] -> do
      case parseExternalContentId identifier of
        Just eid -> return $ ITag eid Nothing
        Nothing -> fail $ "Invalid external identifier: " <> T.unpack identifier
    _ -> fail "Invalid ITag format"


-- | Parse an ExternalContentId from text
parseExternalContentId :: Text -> Maybe ExternalContentId
parseExternalContentId t = case T.splitOn ":" t of
  ["isbn", isbn] -> Just $ IsbnId isbn
  ["podcast", "guid", guid] -> Just $ PodcastGuidId guid
  ["podcast", "item", "guid", guid] -> Just $ PodcastItemGuidId guid
  ["podcast", "publisher", "guid", guid] -> Just $ PodcastPublisherGuidId guid
  ["isan", isan] -> Just $ IsanId isan
  ["geo", geohash] -> Just $ GeohashId geohash
  ["doi", doi] -> Just $ DoiId (T.toLower doi)
  [tag] | T.head tag == '#' -> Just $ HashtagId (T.toLower tag)
  _ -> Nothing


-- | Convert ExternalContentId to text
externalContentIdToText :: ExternalContentId -> Text
externalContentIdToText = \case
  UrlId url -> url
  HashtagId tag -> tag
  GeohashId hash -> "geo:" <> hash
  IsbnId isbn -> "isbn:" <> isbn
  PodcastGuidId guid -> "podcast:guid:" <> guid                     -- Podcast Feeds
  PodcastItemGuidId guid -> "podcast:item:guid:" <> guid            -- Podcast Episodes
  PodcastPublisherGuidId guid -> "podcast:publisher:guid:" <> guid  -- Podcast Publishers
  IsanId isan -> "isan:" <> isan                                    -- Movies
  DoiId doi -> "doi:" <> doi                                        -- Papers

-- | FromJSON instance for ExternalContentId
instance FromJSON ExternalContentId where
  parseJSON = withText "ExternalContentId" $ \t ->
    case parseExternalContentId t of
      Just eid -> return eid
      Nothing -> fail $ "Invalid external identifier: " <> T.unpack t

-- | ToJSON instance for ExternalContentId
instance ToJSON ExternalContentId where
  toEncoding = text . externalContentIdToText
  toJSON = String . externalContentIdToText

-- | Parses a maybe pubkey from a JSON value.
parseMaybePubKey :: Value -> Parser (Maybe PubKeyXO)
parseMaybePubKey (String s) | T.null s = return Nothing
parseMaybePubKey (String s) = case decodeHex s of
    Just bs | BS.length bs == 32 -> return $ importPubKeyXO bs
    _ -> fail "Invalid pubkey format"
parseMaybePubKey Null = return Nothing
parseMaybePubKey _ = fail "Expected string or null for pubkey"
