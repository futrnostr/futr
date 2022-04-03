{-# LANGUAGE OverloadedStrings     #-}

module NostrFunctions where

import qualified Crypto.Hash.SHA256     as SHA256
import           Crypto.Schnorr         (KeyPair, Msg, SchnorrSig, XOnlyPubKey,
                                         verifyMsgSchnorr)
import qualified Crypto.Schnorr as Schnorr
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (toStrict)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, pack, unpack)
import           Data.DateTime
import           GHC.Exts               (fromList)

import NostrTypes

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

isPost :: Event -> Bool
isPost e = kind e == 1

eventToPost :: Event -> Maybe Post
eventToPost e =
    case kind e of
        1 ->
            Just $ Post
                { postId = eventId e
                , author = pack $ Schnorr.exportXOnlyPubKey $ pubKey e
                , postContent = content e
                , posted = created_at e
                }
        _ ->
            Nothing

eventFilter :: XOnlyPubKey -> [XOnlyPubKey] -> Value
eventFilter p followers =
    Array $ fromList
        [ object $ fromList -- notes, profiles and contact lists of people we follow (and ourselves)
            [ ( "kinds"   , Array $ fromList $ [Number 0, Number 1, Number 2, Number 3])
            , ("authors"  , Array $ fromList $ map String $ map (pack . Schnorr.exportXOnlyPubKey) followers)
            ]
        , object $ fromList  -- posts mentioning us and direct messages to us
            [ ( "kinds"   , Array $ fromList [Number 1, Number 4])
            , ("#p"       , Array $ fromList [String $ pack $ Schnorr.exportXOnlyPubKey p])
            ]
        , object $ fromList -- our own direct messages to other people
            [ ( "kinds"   , Array $ fromList [Number 4])
            , ("authors"  , Array $ fromList [String $ pack $ Schnorr.exportXOnlyPubKey p])
            ]
        ]
