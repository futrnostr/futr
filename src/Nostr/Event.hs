{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Nostr.Event where

import           Control.Monad          (mzero)
import qualified Crypto.Hash.SHA256     as SHA256
import           Crypto.Schnorr
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Lazy   (toStrict)
import           Data.DateTime
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, toLower, pack, unpack)
import qualified Data.Vector            as V
import           GHC.Exts               (fromList)

import Nostr.Keys
import Nostr.Kind
import Nostr.Profile (RelayURL)
import Nostr.Relay

newtype EventId = EventId
  { getEventId :: ByteString
  }
  deriving (Eq)

data Marker = Reply | Root
  deriving (Eq, Show)

data Tag
  = ETag EventId (Maybe RelayURL) (Maybe Marker)
  | PTag UnknownXOnlyPubKey (Maybe RelayURL) (Maybe ProfileName)
  | NonceTag
  | UnknownTag
  deriving (Eq, Show)

data Event = Event
  { eventId    :: EventId
  , pubKey     :: XOnlyPubKey
  , created_at :: DateTime
  , kind       :: Kind
  , tags       :: [Tag]
  , content    :: Text
  , sig        :: SchnorrSig
  }
  deriving (Eq, Show)

data UnsignedEvent = UnsignedEvent
  { pubKey'     :: XOnlyPubKey
  , created_at' :: DateTime
  , kind'       :: Kind
  , tags'       :: [Tag]
  , content'    :: Text
  }
  deriving (Eq, Show)

type ReceivedEvent = (Event, [Relay])

instance Show EventId where
  showsPrec _ = shows . B16.encodeBase16 . getEventId

instance FromJSON EventId where
  parseJSON = withText "EventId" $ \i -> do
    case eventId' i of
      Just e -> return e
      _      -> fail "invalid event id"

instance ToJSON EventId where
  toJSON e = String $ B16.encodeBase16 $ getEventId e

instance FromJSON Event where
  parseJSON = withObject "event data" $ \e -> Event
    <$> e .: "id"
    <*> e .: "pubkey"
    <*> (fromSeconds <$> e .: "created_at")
    <*> e .: "kind"
    <*> e .: "tags"
    <*> e .: "content"
    <*> e .: "sig"

instance ToJSON Event where
  toJSON Event {..} = object
     [ "id"         .= exportEventId eventId
     , "pubkey"     .= exportXOnlyPubKey pubKey
     , "created_at" .= toSeconds created_at
     , "kind"       .= kind
     , "tags"       .= tags
     , "content"    .= content
     , "sig"        .= exportSchnorrSig sig
     ]

instance ToJSON UnsignedEvent where
  toJSON (UnsignedEvent {..}) = Array $ fromList
     [ Number 0
     , String $ pack $ exportXOnlyPubKey $ pubKey'
     , Number $ fromIntegral $ toSeconds $ created_at'
     , toJSON kind'
     , toJSON tags'
     , toJSON content'
     ]

instance FromJSON Tag where
 parseJSON (Array v)
   | V.length v > 0 =
       case v V.! 0 of
         String "e" ->
           ETag <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2) <*> parseJSON (v V.! 3)
         String "p" ->
           PTag <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2) <*> parseJSON (v V.! 3)
         _ ->
           return UnknownTag
   | otherwise = return UnknownTag
 parseJSON _ = return UnknownTag

instance ToJSON Tag where
 toJSON (ETag eventId Nothing Nothing) =
   Array $ fromList
     [ String "e"
     , String $ B16.encodeBase16 $ getEventId eventId
     ]
 toJSON (ETag eventId relayURL Nothing) =
   Array $ fromList
     [ String "e"
     , String $ B16.encodeBase16 $ getEventId eventId
     , case relayURL of
         Just relayURL' ->
           String relayURL'
         Nothing ->
           Null
     ]
 toJSON (ETag eventId relayURL marker) =
   Array $ fromList
     [ String "e"
     , String $ B16.encodeBase16 $ getEventId eventId
     , case relayURL of
         Just relayURL' ->
           String relayURL'
         Nothing ->
           Null
     , case marker of
         Just Reply ->
           String "reply"
         Just Root ->
           String "root"
         Nothing ->
           Null
     ]
 toJSON (PTag xo relayURL name) =
   Array $ fromList
     [ String "p"
     , case xo of
         ValidXOnlyPubKey xo' ->
           toJSON xo'
         InvalidXOnlyPubKey ->
           Null
     , toJSON relayURL
     , toJSON name
     ]
 toJSON _ = -- @todo implement nonce tag
   Array $ fromList []

instance FromJSON Marker where
 parseJSON = withText "Marker" $ \m -> do
   case toLower m of
     "reply" -> return Reply
     "root"  -> return Root
     _       -> mzero

instance ToJSON Marker where
 toJSON (Reply) = String "reply"
 toJSON (Root) = String "root"

eventId' :: Text -> Maybe EventId
eventId' t = do
  bs <- decodeHex t
  case BS.length bs of
    32 -> Just $ EventId bs
    _  -> Nothing

exportEventId :: EventId -> String
exportEventId i = unpack . B16.encodeBase16 $ getEventId i

signEvent :: UnsignedEvent -> KeyPair -> XOnlyPubKey -> Event
signEvent u kp xo =
  Event
    { eventId = eid
    , pubKey = xo
    , created_at = created_at' u
    , kind = kind' u
    , tags = tags' u
    , content = content' u
    , sig = s
    }
  where
    eid = EventId {getEventId = SHA256.hash $ toStrict $ encode u}
    s = signMsgSchnorr kp $ fromJust $ msg $ getEventId eid

textNote :: Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
textNote note xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = TextNote, tags' = [], content' = note}
