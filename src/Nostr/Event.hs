{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Nostr.Event where

import           Control.Monad          (mzero)
import qualified Crypto.Hash.SHA256     as SHA256
import           Data.Aeson
import           Data.Aeson.Text        (encodeToTextBuilder)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Data.Text              (Text, toLower, pack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as LazyText
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Vector            as V
import           GHC.Exts               (fromList)

import Nostr.Keys
import Nostr.Kind
import Nostr.Profile (Profile(..), RelayURL, DisplayName)
import Nostr.Relay

newtype EventId = EventId { getEventId :: ByteString } deriving (Eq)

data Relationship = Reply | Root
  deriving (Eq, Show)

data Tag
  = ETag EventId (Maybe RelayURL) (Maybe Relationship)
  | PTag PubKeyXO (Maybe RelayURL) (Maybe DisplayName)
  | NonceTag
  | UnknownTag
  deriving (Eq, Show)

data Event = Event
  { eventId    :: EventId
  , pubKey     :: PubKeyXO
  , created_at :: Int64
  , kind       :: Kind
  , tags       :: [Tag]
  , content    :: Text
  , sig        :: Signature
  }
  deriving (Eq, Show)

data UnsignedEvent = UnsignedEvent
  { pubKey'     :: PubKeyXO
  , created_at' :: Int64
  , kind'       :: Kind
  , tags'       :: [Tag]
  , content'    :: Text
  }
  deriving (Eq, Show)

type ReceivedEvent = (Event, [Relay])

type Contact = (PubKeyXO, Maybe DisplayName)

instance Show EventId where
  showsPrec _ = shows . B16.encode . getEventId

instance FromJSON EventId where
  parseJSON = withText "EventId" $ \i -> do
    case eventId' i of
      Just e -> return e
      _      -> fail "invalid event id"

instance ToJSON EventId where
  toJSON e = String $ decodeUtf8 . B16.encode $ getEventId e

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
  toJSON Event {..} = object
     [ "id"         .= exportEventId eventId
     , "pubkey"     .= show pubKey
     , "created_at" .= created_at
     , "kind"       .= kind
     , "tags"       .= tags
     , "content"    .= content
     , "sig"        .= exportSignature sig
     ]

instance ToJSON UnsignedEvent where
  toJSON (UnsignedEvent {..}) = Array $ fromList
     [ Number 0
     , String $ pack $ show pubKey'
     , Number $ fromIntegral $ created_at'
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
     , maybe (String "") (\r -> String r) relayURL
     ]
 toJSON (ETag eventId relayURL marker) =
   Array $ fromList
     [ String "e"
     , String $ decodeUtf8 $ B16.encode $ getEventId eventId
     , maybe (String "") (\r -> String r) relayURL
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
     , maybe (String "") (\r -> String r) relayURL
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

eventId' :: Text -> Maybe EventId
eventId' t = do
  bs <- decodeHex t
  case BS.length bs of
    32 -> Just $ EventId bs
    _  -> Nothing

exportEventId :: EventId -> String
exportEventId = show

signEvent :: UnsignedEvent -> KeyPair -> Maybe Event
signEvent u kp =
  Event eid (pubKey' u) (created_at' u) (kind' u) (tags' u) (content' u) <$> schnorrSign kp (getEventId eid)
  where
    eid = EventId { getEventId = SHA256.hash $ toStrict $ encode u }

validateEventId :: Event -> Bool
validateEventId e = (getEventId $ eventId e) == (SHA256.hash $ toStrict $ encode e)

verifySignature :: Event -> Bool -- @todo: implement delagate verification (subkeys?)
verifySignature e = schnorrVerify (pubKey e) (toStrict $ encode e) (sig e)

textNote :: Text -> PubKeyXO -> Int64 -> UnsignedEvent
textNote note xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = ShortTextNote
    , tags' = []
    , content' = note
    }

setMetadata :: Profile -> PubKeyXO -> Int64 -> UnsignedEvent
setMetadata profile xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = Metadata
    , tags' = []
    , content' = LazyText.toStrict . toLazyText . encodeToTextBuilder . toJSON $ profile
    }

readProfile :: Event -> Maybe Profile
readProfile event = case kind event of
  Metadata ->
    decode $ fromStrict $ encodeUtf8 $ content event
  _ ->
    Nothing

replyNote :: Event -> Text -> PubKeyXO -> Int64 -> UnsignedEvent
replyNote event note xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = ShortTextNote
    , tags' = [ETag (eventId event) Nothing (Just Reply)]
    , content' = note
    }

setContacts :: [(PubKeyXO, Maybe DisplayName)] -> PubKeyXO -> Int64 -> UnsignedEvent
setContacts contacts xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = FollowList
    , tags' = map (\c -> PTag (fst c) (Just "") (snd c)) contacts
    , content' = ""
    }

deleteEvents :: [EventId] -> Text -> PubKeyXO -> Int64 -> UnsignedEvent
deleteEvents eids reason xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = EventDeletion
    , tags' = toDelete
    , content' = reason
    }
  where
    toDelete = map (\eid -> ETag eid Nothing Nothing) eids

getReplyEventId :: Event -> Maybe EventId
getReplyEventId = getRelationshipEventId Reply

getRootEventId :: Event -> Maybe EventId
getRootEventId = getRelationshipEventId Root

getRelationshipEventId :: Relationship -> Event -> Maybe EventId
getRelationshipEventId m e =
  if null replyList
    then Nothing
    else Just $ extractEventId $ head replyList
  where
    replyFilter :: Relationship -> Tag -> Bool
    replyFilter m' (ETag _ _ (Just m'')) = m' == m''
    replyFilter _ _ = False

    replyList = filter (replyFilter m) $ tags e

    extractEventId :: Tag -> EventId
    extractEventId (ETag eid _ _) = eid
    extractEventId _ = error "Could not extract event id from reply or root tag"

decodeHex :: ConvertibleStrings a ByteString => a -> Maybe ByteString
decodeHex str =
  case B16.decode $ cs str of
    Right bs -> Just bs
    Left _   -> Nothing
