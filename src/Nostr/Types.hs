-- | Module: Nostr.Types
-- Defines types related to the Nostr protocol.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Types where

import Control.Monad (mzero)
import Data.Aeson hiding (Error)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Default
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text, isInfixOf, toLower, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import Text.URI (URI, mkURI, render)
import Data.Aeson.Types (Parser)

import Nostr.Keys (PubKeyXO, Signature, exportSignature)

-- | Represents a wrapped URI used within a relay.
newtype RelayURI = RelayURI URI deriving (Eq, Ord, Show)

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
  deriving (Eq, Show)

-- | Represents a subscription id as text.
type SubscriptionId = Text

-- | Represents a subscription.
data Subscription = Subscription
  { filters :: [Filter]
  , subId   :: SubscriptionId
  }
  deriving (Eq, Show)

data Filter
  = MetadataFilter [PubKeyXO] Int
  | FollowListFilter [PubKeyXO] Int
  | ShortTextNoteFilter [PubKeyXO] Int
  | LinkedEvents [EventId] Int
  | AllNotes Int
  | AllMetadata Int
  deriving (Eq, Show)

data Request
  = SendEvent Event
  | Subscribe Subscription
  | Close SubscriptionId
  | Disconnect
  deriving (Eq, Show)

data Response
  = EventReceived SubscriptionId Event
  | Ok EventId Bool Text
  | Eose SubscriptionId
  | Closed SubscriptionId Text
  | Notice Text
  deriving (Eq, Show)

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
  deriving (Eq, Show)

newtype EventId = EventId { getEventId :: ByteString } deriving (Eq, Ord)

data Relationship = Reply | Root
  deriving (Eq, Show)

data Tag
  = ETag EventId (Maybe RelayURI) (Maybe Relationship)
  | PTag PubKeyXO (Maybe RelayURI) (Maybe DisplayName)
  | NonceTag
  | UnknownTag
  deriving (Eq, Show)

data Event = Event
  { eventId    :: EventId
  , pubKey     :: PubKeyXO
  , createdAt :: Int64
  , kind       :: Kind
  , tags       :: [Tag]
  , content    :: Text
  , sig        :: Signature
  }
  deriving (Eq, Show)

data UnsignedEvent = UnsignedEvent
  { pubKey'     :: PubKeyXO
  , createdAt' :: Int64
  , kind'       :: Kind
  , tags'       :: [Tag]
  , content'    :: Text
  }
  deriving (Eq, Show)

type ReceivedEvent = (Event, [Relay])

type Contact = (PubKeyXO, Maybe DisplayName)

type Name = Text
type DisplayName = Text
type About = Text
type Picture = Text
type Banner = Text
type Nip05 = Text

data Profile = Profile
  { name :: Maybe Text
  , displayName :: Maybe Text
  , about :: Maybe Text
  , picture :: Maybe Text
  , nip05 :: Maybe Text
  , banner :: Maybe Text
  } deriving (Eq, Show)

-- Helper functions

noticeReason :: Text -> StandardPrefix
noticeReason errMsg
  | "duplicate:"    `isInfixOf` errMsg = Duplicate
  | "pow:"          `isInfixOf` errMsg = Pow
  | "blocked:"      `isInfixOf` errMsg = Blocked
  | "rate-limited:" `isInfixOf` errMsg = RateLimited
  | "invalid:"      `isInfixOf` errMsg = Invalid
  | otherwise                          = Error

decodeHex :: ConvertibleStrings a ByteString => a -> Maybe ByteString
decodeHex str =
  case B16.decode $ cs str of
    Right bs -> Just bs
    Left _   -> Nothing

-- | Unwrap relay URI.
unwrapRelayURI :: RelayURI -> URI
unwrapRelayURI (RelayURI u) = u

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
  toJSON e = String $ decodeUtf8 . B16.encode $ getEventId e

instance FromJSON Event where
  parseJSON = withObject "event data" $ \e -> Event
    <$> e .: "id"
    <*> e .: "pubkey"
    <*> e .: "createdAt"
    <*> e .: "kind"
    <*> e .: "tags"
    <*> e .: "content"
    <*> e .: "sig"

instance ToJSON Event where
  toJSON Event {..} = object
     [ "id"         .= show eventId
     , "pubkey"     .= show pubKey
     , "createdAt" .= createdAt
     , "kind"       .= kind
     , "tags"       .= tags
     , "content"    .= content
     , "sig"        .= exportSignature sig
     ]

instance ToJSON UnsignedEvent where
  toJSON (UnsignedEvent {..}) = Array $ fromList
     [ Number 0
     , String $ pack $ show pubKey'
     , Number $ fromIntegral $ createdAt'
     , toJSON kind'
     , toJSON tags'
     , toJSON content'
     ]

instance FromJSON Tag where
 parseJSON (Array v)
   | V.length v > 0 =
       case v V.! 0 of
         String "e" ->
           ETag <$> parseJSON (v V.! 1) <*> parseJSON (fromMaybe Null $ v V.!? 2) <*> parseJSON (fromMaybe Null $ v V.!? 3)
         String "p" ->
           PTag <$> parseJSON (v V.! 1) <*> parseJSON (fromMaybe Null $ v V.!? 2) <*> parseJSON (fromMaybe Null $ v V.!? 3)
         _ ->
           return UnknownTag
   | otherwise = return UnknownTag
 parseJSON _ = return UnknownTag

instance ToJSON Tag where
 toJSON (ETag eventId Nothing Nothing) =
   Array $ fromList
     [ String "e"
     , String $ decodeUtf8 $ B16.encode $ getEventId eventId
     ]
 toJSON (ETag eventId relayURL Nothing) =
   Array $ fromList
     [ String "e"
     , String $ decodeUtf8 $ B16.encode $ getEventId eventId
     , maybe (String "") (\r -> String $ render $ unwrapRelayURI r) relayURL
     ]
 toJSON (ETag eventId relayURL marker) =
   Array $ fromList
     [ String "e"
     , String $ decodeUtf8 $ B16.encode $ getEventId eventId
     , maybe (String "") (\r -> String $ render $ unwrapRelayURI r) relayURL
     , case marker of
         Just Reply ->
           String "reply"
         Just Root ->
           String "root"
     ]
 toJSON (PTag xo relayURL name) =
   Array $ fromList
     [ String "p"
     , toJSON xo
     , maybe (String "") (\r -> String $ render $ unwrapRelayURI r) relayURL
     , maybe (String "") (\n -> String n) name
     ]
 toJSON _ = -- @todo implement nonce tag
   Array $ fromList []

instance FromJSON Relationship where
 parseJSON = withText "Relationship" $ \m -> do
   case toLower m of
     "reply" -> return Reply
     "root"  -> return Root
     _       -> mzero

instance ToJSON Relationship where
 toJSON (Reply) = String "reply"
 toJSON (Root) = String "root"

instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0 :: Parser Text
    case type' of
      "EVENT" -> do
        subId' <- parseJSON $ arr V.! 1
        event <- parseJSON $ arr V.! 2
        return $ EventReceived subId' event
      "OK" -> do
        id' <- parseJSON $ arr V.! 2
        bool <- parseJSON $ arr V.! 3
        message <- parseJSON $ arr V.! 4
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

instance ToJSON Request where
  toJSON sr = case sr of
    SendEvent e -> Array $ fromList
       [ String $ pack "EVENT"
       , toJSON e
       ]

    Subscribe (Subscription efs s) -> Array $ fromList
      ([ String $ pack "REQ"
      , String $ s
       ] ++ map (\ef -> toJSON ef) efs)

    Close subId' -> Array $ fromList
       [ String $ pack "CLOSE"
       , String subId'
       ]

    Disconnect -> String $ pack "Bye!"

-- | Converts a `RelayURI` into its JSON representation.
instance FromJSON RelayURI where
  parseJSON = withText "URI" $ \u ->
    case mkURI u of
      Just u' -> return $ RelayURI u'
      Nothing -> fail "Invalid relay URI"

-- | Parses a JSON value into a `RelayURI`.
instance ToJSON RelayURI where
  toJSON (RelayURI u) = String $ render u

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
  toJSON r = object $ fromList
    [ ( "uri", String $ render $ unwrapRelayURI $ uri r)
    , ( "info", toJSON $ info r)
    ]

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
  toJSON Metadata      = Number 0
  toJSON ShortTextNote = Number 1
  toJSON FollowList    = Number 3
  toJSON EventDeletion = Number 5
  toJSON Repost        = Number 6
  toJSON Reaction      = Number 7
  toJSON Seal          = Number 13
  toJSON DirectMessage = Number 14

instance ToJSON Filter where
  toJSON (MetadataFilter xos now) = object
    [ "kinds" .= toJSON [Metadata]
    , "authors" .= toJSON xos
    , "limit" .= Number 1
    , "until" .= toJSON (now + 60)
    ]
  toJSON (FollowListFilter xos now) = object
    [ "kinds" .= toJSON [FollowList]
    , "authors" .= toJSON xos
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]
  toJSON (ShortTextNoteFilter xos now) = object
    [ "kinds" .= toJSON [ShortTextNote, EventDeletion]
    , "authors" .= toJSON xos
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]
  toJSON (LinkedEvents eids now) = object
    [ "kinds" .= toJSON [ShortTextNote]
    , "limit" .= Number 500
    , "#e" .= toJSON eids
    , "until" .= toJSON (now + 60)
    ]
  toJSON (AllNotes now) = object
    [ "kinds" .= toJSON [ShortTextNote]
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]
  toJSON (AllMetadata now) = object
    [ "kinds" .= toJSON [Metadata]
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]

instance Default Profile where
  def = Profile Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON Profile where
  toJSON (Profile name displayName about picture nip05 banner) = object
    [ "name" .= toJSON name
    , "display_name" .= toJSON displayName
    , "about" .= toJSON about
    , "picture" .= toJSON picture
    , "nip05" .= toJSON nip05
    , "banner" .= toJSON banner
    ]

instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .: "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
    <*> e .:? "nip05"
    <*> e .:? "banner"
