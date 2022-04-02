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
import           Data.DateTime
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

defaultPool :: [Relay]
defaultPool =
    [ Relay
        { host = "nostr.rocks"
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
{-
    [ Relay
        { host = "localhost"
        , port = 2700
        , readable = True
        , writable = True
        , connected = False
        }
    ]
-}

type RelayURL = Text

newtype EventId =
  EventId
    { getEventId :: ByteString
    }
  deriving (Eq)

data ServerRequest
    = SendEvent Event
--    | Request Text [Filter]
    | Close Text

instance ToJSON ServerRequest where
    toJSON sr = case sr of
        (SendEvent e) -> Array $ fromList
             [ String $ pack "EVENT"
             , toJSON e
             ]
        (Close subid) -> Array $ fromList
             [ String $ pack "CLOSE"
             , String subid
             ]

instance Show EventId where
  showsPrec _ = shows . B16.encodeBase16 . getEventId

instance ToJSON EventId where
  toJSON e = String $ pack $ exportEventId e

instance ToJSON SchnorrSig where
    toJSON s = String $ pack $ Schnorr.exportSchnorrSig s

instance ToJSON XOnlyPubKey where
    toJSON x = String $ pack $ Schnorr.exportXOnlyPubKey x

instance FromJSON EventId where
  parseJSON =
    withText "EventId" $ \i -> do
      case eventId' i of
        Just e -> return e
        _      -> fail "invalid event id"

instance FromJSON SchnorrSig where
    parseJSON =
        withText "SchnorrSig" $ \s -> do
            case (textToByteStringType s Schnorr.schnorrSig) of
                Just s' -> return s'
                _       -> fail "invalid schnorr sig"

textToByteStringType :: Text -> (ByteString -> Maybe a) -> Maybe a
textToByteStringType t f =
    case Schnorr.decodeHex t of
        Just bs -> f bs
        Nothing -> Nothing

eventId' :: Text -> Maybe EventId
eventId' t = do
  bs <- Schnorr.decodeHex t
  case BS.length bs of
    32 -> Just $ EventId bs
    _  -> Nothing

instance FromJSON XOnlyPubKey where
  parseJSON =
    withText "XOnlyPubKey" $ \p -> do
      case (textToByteStringType p Schnorr.xOnlyPubKey) of
        Just e -> return e
        _      -> fail "invalid XOnlyPubKey"

exportEventId :: EventId -> String
exportEventId i = unpack . B16.encodeBase16 $ getEventId i

data Event =
  Event
    { eventId    :: EventId
    , pubKey     :: XOnlyPubKey
    , created_at :: DateTime
    , kind       :: Int
    , tags       :: [Tag]
    , content    :: Text
    , sig        :: SchnorrSig
    }
  deriving (Eq, Show)

instance ToJSON Event where
    toJSON Event {..} = object
         [ "id"         .= exportEventId eventId
         , "pubKey"     .= Schnorr.exportXOnlyPubKey pubKey
         , "created_at" .= toSeconds created_at
         , "kind"       .= kind
         , "tags"       .= tags
         , "content"    .= content
         , "sig"        .= Schnorr.exportSchnorrSig sig
         ]

instance FromJSON Event where
    parseJSON = withObject "event data" $ \e -> Event
        <$> e .: "id"
        <*> e .: "pubKey"
        <*> (fromSeconds <$> e .: "created_at")
        <*> e .: "kind"
        <*> e .: "tags"
        <*> e .: "content"
        <*> e .: "sig"

data RawEvent =
  RawEvent
    { pubKey'     :: XOnlyPubKey
    , created_at' :: DateTime
    , kind'       :: Int
    , tags'       :: [Tag]
    , content'    :: Text
    }
  deriving (Eq, Show)

data Tag
  = ETag (EventId, RelayURL)
  | PTag (XOnlyPubKey, RelayURL)
  deriving (Eq, Show)

instance FromJSON Tag where
  parseJSON (Array v)
    | V.length v == 3 =
      case v V.! 0 of
        String "p" ->
          ETag <$> ((,) <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2))
        String "e" ->
          PTag <$> ((,) <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2))
        _ -> mzero
    | otherwise = mzero
  parseJSON _ = mzero

instance ToJSON Tag where
    toJSON (ETag (eventId, relayURL)) =
        Array $ fromList [String "e", String $ pack $ exportEventId eventId, String $ relayURL]
    toJSON (PTag (xOnlyPubKey, relayURL)) =
        Array $ fromList [String "p", String $ pack $ Schnorr.exportXOnlyPubKey xOnlyPubKey, String $ relayURL]

serializeEvent :: Event -> ByteString
serializeEvent e =
  toStrict $
  encode $
  Array $
  fromList $
  [ Number 0
  , String $ pack $ Schnorr.exportXOnlyPubKey $ pubKey e
  , Number $ fromIntegral $ toSeconds $ created_at e
  , Number $ fromIntegral $ kind e
  , serializeTags $ tags e
  , String $ content e
  ]

serializeRawEvent :: RawEvent -> ByteString
serializeRawEvent e =
  toStrict $
  encode $
  Array $
  fromList $
  [ Number 0
  , String $ pack $ Schnorr.exportXOnlyPubKey $ pubKey' e
  , Number $ fromIntegral $ toSeconds $ created_at' e
  , Number $ fromIntegral $ kind' e
  , serializeTags $ tags' e
  , String $ content' e
  ]

serializeTags :: [Tag] -> Value
serializeTags ts = Array $ fromList $ map serializeTag ts

serializeTag :: Tag -> Value
serializeTag (ETag (i, r)) =
  Array $
  fromList
    [ String $ pack "e"
    , String $ pack $ exportEventId i
    , String r
    ]
serializeTag (PTag (p, r)) =
  Array $
  fromList
    [ String $ pack "p"
    , String $ pack $ Schnorr.exportXOnlyPubKey p
    , String r
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

setMetadata :: Text -> Text -> Text -> XOnlyPubKey -> DateTime -> RawEvent
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

textNote :: Text -> XOnlyPubKey -> DateTime -> RawEvent
textNote note xo t =
  RawEvent
    {pubKey' = xo, created_at' = t, kind' = 1, tags' = [], content' = note}

recommendServer :: RelayURL -> XOnlyPubKey -> DateTime -> RawEvent
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
