{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module NostrTypes where

import           Control.Monad          (mzero, (<=<))
import qualified Crypto.Hash.SHA256     as SHA256
import           Crypto.Schnorr         (KeyPair, Msg, SchnorrSig, XOnlyPubKey,
                                         verifyMsgSchnorr)
import qualified Crypto.Schnorr         as Schnorr
import           Data.Aeson
import           Data.Aeson.Encoding    as AE
import           Data.Aeson.Types       as AesonTypes
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Lazy   (toStrict)
import           Data.Default
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, pack, unpack)
import qualified Data.Vector            as V
import           Foreign.C.Types        (CTime (..))
import           GHC.Exts               (fromList)
import           GHC.Generics           (Generic)
import           Network.Socket         (PortNumber)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Posix.Types     (EpochTime)

data Relay =
  Relay
    { host      :: String
    , port      :: PortNumber
    , readable  :: Bool
    , writable  :: Bool
    , connected :: Bool
    }
  deriving (Eq, Show)

type Pool = [Relay]

instance Default Pool where
  def =
    [ Relay
        { host = "relayer.fiatjaf.com"
        , port = 443
        , readable = True
        , writable = True
        , connected = False
        }
    , Relay
        { host = "nostr-pub.wellorder.net"
        , port = 443
        , readable = True
        , writable = True
        , connected = False
        }
    ]

type RelayURL = Text

newtype EventId =
  EventId
    { getEventId :: ByteString
    }
  deriving (Eq)

instance Show EventId where
  showsPrec _ = shows . B16.encodeBase16 . getEventId

instance ToJSON EventId where
  toJSON e = AesonTypes.String $ pack $ exportEventId e

instance FromJSON EventId where
  parseJSON =
    withText "EventId" $ \i -> do
      case eventId' i of
        Just e -> return e
        _      -> fail "invalid event id"

eventId' :: Text -> Maybe EventId
eventId' t = do
  bs <- Schnorr.decodeHex t
  case BS.length bs of
    32 -> Just $ EventId bs
    _  -> Nothing

instance FromJSON XOnlyPubKey where
  parseJSON =
    withText "XOnlyPubKey" $ \p -> do
      case xOnlyPubKey' p of
        Just e -> return e
        _      -> fail "invalid XOnlyPubKey"

xOnlyPubKey' :: Text -> Maybe XOnlyPubKey
xOnlyPubKey' t = do
  bs <- Schnorr.decodeHex t
  case BS.length bs of
    32 -> Schnorr.xOnlyPubKey bs
    _  -> Nothing

exportEventId :: EventId -> String
exportEventId i = unpack . B16.encodeBase16 $ getEventId i

data Event =
  Event
    { eventId    :: EventId
    , pubKey     :: XOnlyPubKey
    , created_at :: EpochTime
    , kind       :: Int
    , tags       :: [Tag]
    , content    :: Text
    , sig        :: SchnorrSig
    }
  deriving (Eq, Show)

data RawEvent =
  RawEvent
    { pubKey'     :: XOnlyPubKey
    , created_at' :: EpochTime
    , kind'       :: Int
    , tags'       :: [Tag]
    , content'    :: Text
    }
  deriving (Eq, Show)

data Tag
  = ETag (EventId, RelayURL)
  | PTag (XOnlyPubKey, RelayURL)
  deriving (Eq, Show)

newtype TagList =
  TagList [Tag]
  deriving (Show)

instance FromJSON TagList where
  parseJSON (AesonTypes.Object o) = TagList <$> (o .: "tags")
  parseJSON _                     = mzero

instance FromJSON Tag where
  parseJSON (AesonTypes.Array v)
    | V.length v == 3 =
      case v V.! 0 of
        AesonTypes.String "p" ->
          ETag <$> ((,) <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2))
        AesonTypes.String "e" ->
          PTag <$> ((,) <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2))
        _ -> mzero
    | otherwise = mzero
  parseJSON _ = mzero

--data RawEvent = RawEvent String EpochTime Int [[String]] String deriving (Eq, Show)
{-
instance ToJSON Event where
    toJSON (Event kind pubKey content tags created_at) =
        object ["kind" .= 0, "pubKey" .= pubKey, "content" .= content, "tags" .= tags, "created_at" .= created_at]
    toEncoding (Event eventId pubKey created_at kind tags content) =
        pairs ("kind" .= 0 <> "pubKey" .= pubKey <> "content" .= content <> "tags" .= tags <> "created_at" .= utSeconds created_at)

instance FromJSON Event where
    parseJSON = withObject "event" $ \e -> do
        eid   <- e .: "id"
        pk    <- e .: "pubKey"
        cr    <- e .: "created_at" -- @todo parse extra
        ki    <- e .: "kind"
        ta    <- e .: "tags"       -- @todo does this work??
        co    <- e .: "content"
        return Event{..}
-}
serializeEvent :: Event -> ByteString
serializeEvent e =
  toStrict $
  encode $
  AesonTypes.Array $
  fromList $
  [ AesonTypes.Number 0
  , AesonTypes.String $ pack $ Schnorr.exportXOnlyPubKey $ pubKey e
  , AesonTypes.Number $ fromIntegral $ epochTimeToSec $ created_at e
  , AesonTypes.Number $ fromIntegral $ kind e
  , serializeTags $ tags e
  , AesonTypes.String $ content e
  ]

serializeRawEvent :: RawEvent -> ByteString
serializeRawEvent e =
  toStrict $
  encode $
  AesonTypes.Array $
  fromList $
  [ AesonTypes.Number 0
  , AesonTypes.String $ pack $ Schnorr.exportXOnlyPubKey $ pubKey' e
  , AesonTypes.Number $ fromIntegral $ epochTimeToSec $ created_at' e
  , AesonTypes.Number $ fromIntegral $ kind' e
  , serializeTags $ tags' e
  , AesonTypes.String $ content' e
  ]

epochTimeToSec :: EpochTime -> Int
epochTimeToSec (CTime i) = fromIntegral i

serializeTags :: [Tag] -> AesonTypes.Value
serializeTags ts = Array $ fromList $ map serializeTag ts

serializeTag :: Tag -> AesonTypes.Value
serializeTag (ETag (i, r)) =
  AesonTypes.Array $
  fromList
    [ AesonTypes.String $ pack "e"
    , AesonTypes.String $ pack $ exportEventId i
    , AesonTypes.String r
    ]
serializeTag (PTag (p, r)) =
  AesonTypes.Array $
  fromList
    [ AesonTypes.String $ pack "p"
    , AesonTypes.String $ pack $ Schnorr.exportXOnlyPubKey p
    , AesonTypes.String r
    ]

validateEvent :: Event -> Bool
validateEvent e = (getEventId $ eventId e) == (SHA256.hash $ serializeEvent e)

verifySignature :: Event -> Bool
verifySignature e =
  case Schnorr.msg $ serializeEvent e of
    Just m  -> Schnorr.verifyMsgSchnorr p s m
    Nothing -> False
  where
    p = pubKey e
    s = sig e

setMetadata :: Text -> Text -> Text -> XOnlyPubKey -> EpochTime -> RawEvent
setMetadata name about picture xo t =
  RawEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = 0
    , tags' = []
    , content' =
        pack $
        "{name:" ++
        unpack name ++
        ",about:" ++ unpack about ++ ",picture:" ++ unpack picture ++ "}"
    }

textNote :: Text -> XOnlyPubKey -> EpochTime -> RawEvent
textNote note xo t =
  RawEvent
    {pubKey' = xo, created_at' = t, kind' = 1, tags' = [], content' = note}

recommendServer :: RelayURL -> XOnlyPubKey -> EpochTime -> RawEvent
recommendServer url xo t =
  RawEvent
    {pubKey' = xo, created_at' = t, kind' = 3, tags' = [], content' = url}

signEvent :: RawEvent -> KeyPair -> XOnlyPubKey -> Event
signEvent r kp xo =
  Event
    { eventId = eid
    , pubKey = xo
    , created_at = created_at' r
    , kind = kind' r
    , tags = tags' r
    , content = content' r
    , sig = s
    }
  where
    eid = EventId {getEventId = SHA256.hash $ serializeRawEvent r}
    s = Schnorr.signMsgSchnorr kp $ fromJust $ Schnorr.msg $ getEventId eid
