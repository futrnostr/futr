{-# LANGUAGE OverloadedStrings     #-}

module NostrFunctions where

import qualified Crypto.Hash.SHA256     as SHA256
import           Crypto.Random.DRBG      (CtrDRBG, genBytes, newGen, newGenIO)
import           Crypto.Schnorr         (KeyPair, Msg, SchnorrSig, XOnlyPubKey,
                                         verifyMsgSchnorr)
import qualified Crypto.Schnorr as Schnorr
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16  as B16
import           Data.ByteString.Lazy   (toStrict)
import           Data.Default
import           Data.Maybe             (fromJust, fromMaybe, maybe)
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

serializeUnsignedEvent :: UnsignedEvent -> ByteString
serializeUnsignedEvent e =
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

serializeRelayURL :: Maybe RelayURL -> Value
serializeRelayURL Nothing = String ""
serializeRelayURL (Just r) = String r

serializeTags :: [Tag] -> Value
serializeTags ts = Array $ fromList $ map serializeTag ts

serializeTag :: Tag -> Value
serializeTag (ETag i Nothing Nothing) =
  Array $
  fromList
    [ String $ pack "e"
    , String $ pack $ exportEventId i
    ]
serializeTag (ETag i r Nothing) =
  Array $
  fromList
    [ String $ pack "e"
    , String $ pack $ exportEventId i
    , serializeRelayURL r
    ]
serializeTag (ETag i r m) =
  Array $
  fromList
    [ String $ pack "e"
    , String $ pack $ exportEventId i
    , serializeRelayURL r
    , toJSON m
    ]
serializeTag (PTag xo r n) =
  Array $
  fromList
    [ String $ pack "p"
    , toJSON xo
    , serializeRelayURL r
    , toJSON n
    ]
serializeTag NonceTag = Array $ fromList []
serializeTag UnknownTag = Array $ fromList []

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

setMetadata :: Text -> Text -> Text -> Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
setMetadata name about picture nip05 xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = 0
    , tags' = []
    , content' = pack $
      "{\"name\":\"" ++ unpack name ++
      "\",\"about\":\"" ++ unpack about ++
      "\",\"picture\":\"" ++ unpack picture ++
      "\",\"nip05\":\"" ++ unpack nip05 ++ "\"}"
    }

textNote :: Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
textNote note xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = 1, tags' = [], content' = note}

replyNote :: Event -> Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
replyNote event note xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = 1, tags' = [ETag (eventId event) Nothing (Just Reply)], content' = note}

setFollowing :: [Profile] -> RelayURL -> XOnlyPubKey -> DateTime -> UnsignedEvent
setFollowing ps r xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = 3, tags' = profilesToTags ps, content' = ""}

deleteEvents :: [EventId] -> Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
deleteEvents eids reason xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = 5, tags' = toDelete, content' = reason}
  where
    toDelete = map (\eid -> ETag eid Nothing Nothing) eids

profilesToTags :: [Profile] -> [Tag]
profilesToTags ps = map (\p -> pd p) ps
  where
    pd (Profile xo r d) = PTag (ValidXOnlyPubKey xo) (Just r) (Just $ pdName d)

tagsToProfiles :: [Tag] -> [Profile]
tagsToProfiles ts = map (\t ->
    Profile (xo t) (fromMaybe "" $ r t) (pd t)
  ) ts'
  where
    ts' = filter (\p -> isPTag p) ts
    xo (PTag (ValidXOnlyPubKey xo) _ _) = xo
    xo _ = error "error transforming tags to profiles, invalid XOnlyPubKey"
    r (PTag _ r _) = r
    r _ = Nothing
    pd (PTag _ _ n) = ProfileData (fromMaybe "" n) "" "" ""
    pd _ = def

isPTag :: Tag -> Bool
isPTag (PTag (ValidXOnlyPubKey xo) _ _) = True
isPTag _ = False

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
    eid = EventId {getEventId = SHA256.hash $ serializeUnsignedEvent u}
    s = Schnorr.signMsgSchnorr kp $ fromJust $ Schnorr.msg $ getEventId eid

isPost :: Event -> Bool
isPost e = kind e == 1

getReplyEventId :: Event -> Maybe EventId
getReplyEventId = getMarkerEventId Reply

getRootEventId :: Event -> Maybe EventId
getRootEventId = getMarkerEventId Root

getMarkerEventId :: Marker -> Event -> Maybe EventId
getMarkerEventId m e =
  if null replyList
    then Nothing
    else Just $ extractEventId $ head replyList
  where
    replyFilter :: Marker -> Tag -> Bool
    replyFilter m (ETag _ _ (Just m')) = m == m'
    replyFilter m _ = False

    replyList = filter (replyFilter m) $ tags e

    extractEventId :: Tag -> EventId
    extractEventId (ETag eid _ _) = eid
    extractEventId _ = error "Could not extract event id from reply or root tag"

genSubscriptionId :: IO Text
genSubscriptionId = do
    gen <- newGenIO :: IO CtrDRBG
    let Right (randomBytes, newGen) = genBytes 32 gen
    return $ B16.encodeBase16 randomBytes

extractEventFromServerResponse :: ServerResponse -> Event
extractEventFromServerResponse (ServerResponse subId event) = event

relayName :: Relay -> String
relayName r = pr ++ h ++ ":" ++ p where
    pr = if secure r then "wss://" else "ws://"
    h = host r
    p = show $ port r

mapProfileToXOnlyPubKey :: Profile -> XOnlyPubKey
mapProfileToXOnlyPubKey (Profile xo _ _) = xo
