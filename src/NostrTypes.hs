{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module NostrTypes where

import           Control.Monad          (mzero, (<=<))
import           Crypto.Schnorr         (KeyPair, Msg, SchnorrSig, XOnlyPubKey,
                                         verifyMsgSchnorr)
import qualified Crypto.Schnorr         as Schnorr
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Lazy   (toStrict)
import           Data.Default
import           Data.Text              (Text, pack, unpack)
import           Data.DateTime
import qualified Data.Vector            as V
import           Foreign.C.Types        (CTime (..))
import           GHC.Exts               (fromList)
import           GHC.Generics           (Generic)
import           Network.Socket         (PortNumber)

data Relay =
  Relay
    { host      :: String
    , port      :: PortNumber
    , secure    :: Bool
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
        , secure = True
        , readable = True
        , writable = True
        , connected = False
        }
    ,  Relay
        { host = "nostr-pub.wellorder.net"
        , port = 443
        , secure = True
        , readable = True
        , writable = True
        , connected = False
        }
    , Relay
        { host = "localhost"
        , port = 2700
        , secure = False
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

data ServerRequest
    = SendEvent Event
    | RequestRelay Text EventFilter
    | Close Text
    | Disconnect Relay
    deriving (Eq, Show)

data ServerResponse = ServerResponse Text Event
    deriving (Eq, Show)

data Post =
    Post
      { postId :: EventId
      , author :: Text
      , postContent :: Text
      , posted :: DateTime
      }
    deriving (Eq, Show)

instance ToJSON ServerRequest where
    toJSON sr = case sr of
        SendEvent e -> Array $ fromList
             [ String $ pack "EVENT"
             , toJSON e
             ]
        RequestRelay s ef -> Array $ fromList
            [ String $ pack "REQ"
            , String $ s
            , toJSON ef
            ]
        Close subid -> Array $ fromList
             [ String $ pack "CLOSE"
             , String subid
             ]
        Disconnect r -> String $ pack "Bye!"

instance Show EventId where
  showsPrec _ = shows . B16.encodeBase16 . getEventId

instance ToJSON KeyPair where
  toJSON e = String $ pack $ Schnorr.exportKeyPair e

instance ToJSON EventId where
  toJSON e = String $ pack $ exportEventId e

instance ToJSON SchnorrSig where
    toJSON s = String $ pack $ Schnorr.exportSchnorrSig s

instance ToJSON XOnlyPubKey where
    toJSON x = String $ pack $ Schnorr.exportXOnlyPubKey x

instance FromJSON KeyPair where
  parseJSON =
    withText "KeyPair" $ \k -> do
      case (textToByteStringType k Schnorr.keypair) of
        Just k' -> return k'
        _       -> fail "invalid key pair"

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

instance FromJSON ServerResponse where
    parseJSON = withArray "ServerResponse Event" $ \arr -> do
        t <- parseJSON $ arr V.! 0
        s <- parseJSON $ arr V.! 1
        e <- parseJSON $ arr V.! 2
        case t of
            String "EVENT" -> return $ ServerResponse s e
            _ -> fail "Invalid ServerResponse did not have EVENT"

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
         , "pubkey"     .= Schnorr.exportXOnlyPubKey pubKey
         , "created_at" .= toSeconds created_at
         , "kind"       .= kind
         , "tags"       .= tags
         , "content"    .= content
         , "sig"        .= Schnorr.exportSchnorrSig sig
         ]

instance FromJSON Event where
    parseJSON = withObject "event data" $ \e -> Event
        <$> e .: "id"
        <*> e .: "pubkey"
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

data EventFilter
    = EventFilter
        { filterPubKey :: XOnlyPubKey
        , followers    :: [XOnlyPubKey]
--        , from         :: DateTime
        }
    deriving (Eq, Show)

instance FromJSON Tag where
  parseJSON (Array v)
    | V.length v == 3 =
      case v V.! 0 of
        String "e" ->
          ETag <$> ((,) <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2))
        String "p" ->
          PTag <$> ((,) <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2))
        _ -> fail "Unknown tag seen"
    | otherwise = fail "Invalid tag length"
  parseJSON _ = fail "Cannot parse tag"

instance ToJSON Tag where
    toJSON (ETag (eventId, relayURL)) =
        Array $ fromList [String "e", String $ pack $ exportEventId eventId, String $ relayURL]
    toJSON (PTag (xOnlyPubKey, relayURL)) =
        Array $ fromList [String "p", String $ pack $ Schnorr.exportXOnlyPubKey xOnlyPubKey, String $ relayURL]

instance ToJSON EventFilter where
    toJSON ef =
        --Array $ fromList
        --    [ object $ fromList -- notes, profiles and contact lists of people we follow (and ourselves)
             object $ fromList -- notes, profiles and contact lists of people we follow (and ourselves)
                [ ( "kinds"   , Array $ fromList $ [Number 0, Number 1, Number 2, Number 3])
                 , ( "authors"  , Array $ fromList $ map String $ map (pack . Schnorr.exportXOnlyPubKey) $ followers ef)
                ]
                {-
            , object $ fromList  -- posts mentioning us and direct messages to us
                [ ( "kinds"   , Array $ fromList [Number 1, Number 4])
                , ( "#p"       , Array $ fromList [String $ pack $ Schnorr.exportXOnlyPubKey $ filterPubKey ef])
                ]
            , object $ fromList -- our own direct messages to other people
                [ ( "kinds"   , Array $ fromList [Number 4])
                , ("authors"  , Array $ fromList [String $ pack $ Schnorr.exportXOnlyPubKey $ filterPubKey ef])
                ]
                -}
--            ]
