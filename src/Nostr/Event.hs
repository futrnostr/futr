-- | Module: Nostr.Event
-- Defines functions related to events in the Nostr protocol.

{-# LANGUAGE RecordWildCards #-}

module Nostr.Event where

import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Aeson.Encoding (list, text)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import System.Random (randomRIO)

--import Nostr.Bech32 (eventToNevent)
import Nostr.Keys
import Nostr.Encryption (decrypt, getConversationKey, encrypt)
import Nostr.Profile (Profile(..))
import Nostr.Relay (Relay, RelayURI, getUri, isInboxCapable, isOutboxCapable)


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
  deriving (Eq, Generic)


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


type Tag = [Text]

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


-- Instance declarations

-- | Converts an 'EventId' to its string representation.
instance Show EventId where
  showsPrec _ eid = showString "EventId: " . shows (B16.encode $ getEventId eid)


-- | Reads an 'EventId' from its string representation.
instance Read EventId where
  readsPrec _ str = case decodeHex str of
    Just bs | BS.length bs == 32 -> [(EventId bs, "")]
    _ -> []


instance Show Marker where
  show = \case
    Reply -> "reply"
    Root -> "root"
    Mention -> "mention"


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


-- | Sign an event.
signEvent :: UnsignedEvent -> KeyPair -> IO (Maybe Event)
signEvent u kp = do
  s <- schnorrSign kp (getEventId eid)
  case s of
    Just s' -> return $ Just $ Event
      { eventId = eid
      , pubKey = keyPairToPubKeyXO kp
      , createdAt = createdAt' u
      , kind = kind' u
      , tags = tags' u
      , content = content' u
      , sig = s'
      }
    Nothing -> return Nothing
  where
    serializedEvent = toStrict $ encode u
    eid = EventId { getEventId = SHA256.hash serializedEvent }


-- | Validate the event ID.
validateEventId :: Event -> Bool
validateEventId e =
  let unsignedEvent = UnsignedEvent (pubKey e) (createdAt e) (kind e) (tags e) (content e)
      serializedEvent = toStrict $ encode unsignedEvent
      computedId = SHA256.hash serializedEvent
      eventId' = getEventId $ eventId e
  in eventId' == computedId


-- | Verify the signature of an event.
verifySignature :: Event -> Bool -- @todo: implement delagate verification (subkeys?)
verifySignature e = schnorrVerify (pubKey e) (getEventId $ eventId e) (sig e)


-- | Validate both the event ID and signature of an event.
validateEvent :: Event -> Bool
validateEvent e = validateEventId e && verifySignature e


-- | Convert EventId to hex-encoded Text
eventIdToHex :: EventId -> Text
eventIdToHex = decodeUtf8 . B16.encode . getEventId

-- | Convert hex-encoded Text to EventId
eventIdFromHex :: Text -> Maybe EventId
eventIdFromHex hex = case B16.decode (encodeUtf8 hex) of
    Right bs -> Just $ EventId bs
    Left _ -> Nothing


-- | Create a comment event (NIP-10) for text notes.
createComment :: Event -> Text -> PubKeyXO -> Int -> UnsignedEvent
createComment originalEvent content' xo createdAt' =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = createdAt'
    , kind' = ShortTextNote
    , tags' = buildTags originalEvent
    , content' = content'
    }
  where
    buildTags :: Event -> [Tag]
    buildTags e =
      let rootTag = case getRootEventId e of
            Just rootId -> [["e", eventIdToHex rootId, "", "root", pubKeyXOToHex $ pubKey e]]
            Nothing -> [["e", eventIdToHex $ eventId e, "", "root", pubKeyXOToHex $ pubKey e]]

          replyTag = [["e", eventIdToHex $ eventId e, "", "reply", pubKeyXOToHex $ pubKey e]]

          originalPTags = [tag | tag@["p", _] <- tags e]

          authorPTag = ["p", pubKeyXOToHex $ pubKey e]
          pTags = if authorPTag `elem` originalPTags
                  then originalPTags
                  else authorPTag : originalPTags
      in
          rootTag ++ replyTag ++ pTags


-- | Create a repost event (kind 6) for text notes.
createRepost :: Event -> RelayURI -> PubKeyXO -> Int -> UnsignedEvent
createRepost event relayUrl xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = Repost
    , tags' = [ ["e", eventIdToHex $ eventId event, relayUrl]
              , ["p", pubKeyXOToHex $ pubKey event]
              ]
    , content' = decodeUtf8 $ toStrict $ encode event
    }


-- | Create a generic repost event (kind 16) for non-text-note events.
createGenericRepost :: Event -> RelayURI -> PubKeyXO -> Int -> UnsignedEvent
createGenericRepost event relayUrl xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = GenericRepost
    , tags' = [ ["e", eventIdToHex $ eventId event, relayUrl]
              , ["p", pubKeyXOToHex $ pubKey event]
              , ["k", pack $ show $ kindToInt $ kind event]
              ]
    , content' = decodeUtf8 $ toStrict $ encode event
    }


-- | Create a short text note event.
createShortTextNote :: Text -> PubKeyXO -> Int -> UnsignedEvent
createShortTextNote note xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = ShortTextNote
    , tags' = []
    , content' = note
    }


-- | Create metadata event.
createMetadata :: Profile -> PubKeyXO -> Int -> UnsignedEvent
createMetadata p xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = Metadata
    , tags' = []
    , content' = decodeUtf8 $ toStrict $ encode p
    }


-- | Create a follow list event.
createFollowList :: [(PubKeyXO, Maybe Text)] -> PubKeyXO -> Int -> UnsignedEvent
createFollowList contacts xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = FollowList
    , tags' = map makeContactTag contacts
    , content' = ""
    }
  where
    makeContactTag (pubKey, maybeAlias) =
      ["p", pubKeyXOToHex pubKey] ++
      [""] ++  -- relay URL (empty in this case)
      [fromMaybe "" maybeAlias]  -- petname/alias


-- | Create a delete event according to NIP-09.
createEventDeletion :: Event -> Text -> PubKeyXO -> Int -> UnsignedEvent
createEventDeletion event reason xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = EventDeletion
    , tags' = toDelete ++ kTags
    , content' = reason
    }
  where
    toDelete = [["e", eventIdToHex $ eventId event]]
    kTags = [["k", pack $ show $ kindToInt $ kind event]]


createRelayListMetadataEvent :: [Relay] -> PubKeyXO -> Int -> UnsignedEvent
createRelayListMetadataEvent relays xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = RelayListMetadata
    , tags' = map makeRelayTag relays
    , content' = ""
    }
  where
    makeRelayTag r = ["r", getUri r] ++
      if isOutboxCapable r && not (isInboxCapable r)
        then ["write"]
      else if isInboxCapable r && not (isOutboxCapable r)
        then ["read"]
      else []


createPreferredDMRelaysEvent :: [RelayURI] -> PubKeyXO -> Int -> UnsignedEvent
createPreferredDMRelaysEvent urls xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = PreferredDMRelays
    , tags' = map (\url -> ["relay", url]) urls
    , content' = ""
    }


createCanonicalAuthentication :: RelayURI -> Text -> PubKeyXO -> Int -> UnsignedEvent
createCanonicalAuthentication r challenge xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = CanonicalAuthentication
    , tags' = [["relay", r], ["challenge", challenge]]
    , content' = ""
    }


-- | Create a rumor from an event.
createRumor :: PubKeyXO -> Int -> [Tag] -> Text -> Rumor
createRumor pubKey' createdAt' tags' content' =
  let rumorId = calculateEventId pubKey' createdAt' tags' content'
  in Rumor
    { rumorId = rumorId
    , rumorPubKey = pubKey'
    , rumorCreatedAt = createdAt'
    , rumorKind = DirectMessage
    , rumorTags = tags'
    , rumorContent = content'
    }
  where
    calculateEventId :: PubKeyXO -> Int -> [Tag] -> Text -> EventId
    calculateEventId pubKey'' createdAt'' tags'' content'' =
      let unsignedEvent = UnsignedEvent pubKey'' createdAt'' DirectMessage tags'' content''
          serializedEvent = toStrict $ encode unsignedEvent
          computedId = SHA256.hash serializedEvent
      in EventId computedId


-- | Create a seal event.
createSeal :: Rumor -> KeyPair -> PubKeyXO -> IO (Maybe Event)
createSeal rumor kp pk = do
  let rumorJson = encode rumor
  nonce <- getRandomBytes 32
  case getConversationKey (keyPairToSecKey kp) pk of
    Nothing -> return Nothing
    Just conversationKey -> do
      case encrypt (decodeUtf8 $ toStrict rumorJson) conversationKey nonce of
        Left _ -> return Nothing
        Right sealContent -> do
          currentTime <- getCurrentTime
          randomOffset <- randomRIO (0, 2 * 24 * 60 * 60 :: Int)
          let timestamp = floor (utcTimeToPOSIXSeconds currentTime) - randomOffset
          let sealEvent = UnsignedEvent
                { pubKey' = keyPairToPubKeyXO kp
                , createdAt' = timestamp
                , kind' = Seal
                , tags' = []
                , content' = sealContent
                }
          signEvent sealEvent kp


-- | Create a gift wrap event.
createGiftWrap :: Event -> PubKeyXO -> IO (Maybe (Event, KeyPair))
createGiftWrap sealEvent recipientPubKey = do
  randomKeyPair <- createKeyPair
  let sealJson = encode sealEvent
  nonce <- getRandomBytes 32
  case getConversationKey (keyPairToSecKey randomKeyPair) recipientPubKey of
    Nothing -> return Nothing
    Just conversationKey -> do
      case encrypt (decodeUtf8 $ toStrict sealJson) conversationKey nonce of
        Left _ -> return Nothing
        Right wrapContent -> do
          currentTime <- getCurrentTime
          let wrapEvent = UnsignedEvent
                { pubKey' = keyPairToPubKeyXO randomKeyPair
                , createdAt' = floor $ utcTimeToPOSIXSeconds currentTime
                , kind' = GiftWrap
                , tags' = [["p"] ++ map pubKeyXOToHex [recipientPubKey]]
                , content' = wrapContent
                }
          signEvent wrapEvent randomKeyPair >>= \case
            Just e -> return $ Just (e, randomKeyPair)
            Nothing -> return Nothing


-- | Unwrap a gift wrap event.
unwrapGiftWrap :: Event -> KeyPair -> IO (Maybe Event)
unwrapGiftWrap wrapEvent kp = do
  let wrapContent = content wrapEvent
      conversationKey = getConversationKey (keyPairToSecKey kp) (pubKey wrapEvent)
  case conversationKey of
    Nothing -> return Nothing
    Just key -> case decrypt key wrapContent of
      Left _ -> return Nothing
      Right decryptedContent -> case eitherDecode (fromStrict $ encodeUtf8 decryptedContent) of
        Left _ -> return Nothing
        Right sealEvent -> return $ Just sealEvent


-- | Unseal a seal event.
unwrapSeal :: Event -> KeyPair -> IO (Maybe Rumor)
unwrapSeal sealEvent kp = do
  let sealContent = content sealEvent
      conversationKey = getConversationKey (keyPairToSecKey kp) (pubKey sealEvent)
  case conversationKey of
    Just key -> case decrypt key sealContent of
      Right decryptedContent -> do
        if not $ validateEvent sealEvent
          then do
            putStrLn $ "Seal event is not valid: " <> show sealEvent
            return Nothing
          else do
            let sealPK = pubKey sealEvent
            case eitherDecode (fromStrict $ encodeUtf8 decryptedContent) of
              Right rumor -> do
                if rumorPubKey rumor == sealPK
                  then return $ Just rumor
                  else do
                    putStrLn $ "Rumor pubkey does not match seal pubkey: " <> show rumor
                    return Nothing
              Left err -> do
                putStrLn $ "Error decoding rumor: " <> err
                return Nothing
      Left err -> do
        putStrLn $ "Error decrypting rumor: " <> err
        return Nothing
    Nothing -> do
      putStrLn "No conversation key found"
      return Nothing


-- | Get the reply event ID.
getReplyEventId :: Event -> Maybe EventId
getReplyEventId = getRelationshipEventId Reply


-- | Get the root event ID.
getRootEventId :: Event -> Maybe EventId
getRootEventId = getRelationshipEventId Root


-- | Get the relationship event ID.
getRelationshipEventId :: Marker -> Event -> Maybe EventId
getRelationshipEventId m e =
  if null replyList
    then Nothing
    else extractEventId $ head replyList
  where
    replyFilter :: Marker -> Tag -> Bool
    replyFilter m' ["e", _, _, m'', _] = show m' == show m''
    replyFilter _ _ = False

    replyList = filter (replyFilter m) $ tags e

    extractEventId :: Tag -> Maybe EventId
    extractEventId ["e", eid, _, _, _] =
      decodeHex (encodeUtf8 eid) >>= Just . EventId
    extractEventId _ = Nothing


-- | Decodes a hex string to a byte string.
decodeHex :: ConvertibleStrings a ByteString => a -> Maybe ByteString
decodeHex str =
  case B16.decode $ cs str of
    Right bs -> Just bs
    Left _   -> Nothing


-- | Check if a short text note is a comment.
isComment :: Event -> Bool
isComment e = kind e == ShortTextNote && hasETags (tags e)
  where
    hasETags :: [Tag] -> Bool
    hasETags = any isETag

    isETag :: Tag -> Bool
    isETag ("e":_) = True
    isETag _ = False


-- | Extract pubkeys from p-tags in the tags list
getAllPubKeysFromPTags :: [Tag] -> [PubKeyXO]
getAllPubKeysFromPTags = concatMap extractPubKey
  where
    extractPubKey ("p":pubkeyHex:_) = case pubKeyXOFromHex pubkeyHex of
        Just pk -> [pk]
        Nothing -> []
    extractPubKey _ = []
