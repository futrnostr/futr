module Nostr.Bech32
    ( secKeyToBech32
    , bech32ToSecKey
    , pubKeyXOToBech32
    , bech32ToPubKeyXO
    , eventToNaddr
    , naddrToEvent
    , eventToNevent
    , neventToEvent
    , pubKeyXOToNprofile
    , nprofileToPubKeyXO
    , nrelayToRelay
    , relayToNrelay
    , bech32ToEventId
    , eventIdToNote
    , noteToEventId
    ) where

import Codec.Binary.Bech32 qualified as Bech32
import Data.Binary.Put (runPut, putWord8, putByteString, putWord32be)
import Data.Binary.Get (runGet, getWord8, getByteString, isEmpty)
import Data.ByteString qualified as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short qualified as BSS
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word (Word8)
import Text.Read (readMaybe)

import Nostr.Event (Event(..), EventId(..), Kind, kindToInt)
import Nostr.Keys (SecKey, PubKeyXO, importPubKeyXO, exportPubKeyXO, importSecKey, exportSecKey)
import Nostr.Relay (RelayURI)


-- | Bech32 encoding for SecKey
secKeyToBech32 :: SecKey -> T.Text
secKeyToBech32 secKey = toBech32 "nsec" (exportSecKey secKey)


-- | Bech32 decoding to SecKey
bech32ToSecKey :: T.Text -> Maybe SecKey
bech32ToSecKey txt = fromBech32 "nsec" txt >>= importSecKey


-- | Bech32 encoding for PubKeyXO
pubKeyXOToBech32 :: PubKeyXO -> T.Text
pubKeyXOToBech32 pubKeyXO = toBech32 "npub" (exportPubKeyXO pubKeyXO)


-- | Bech32 decoding to PubKeyXO
bech32ToPubKeyXO :: T.Text -> Maybe PubKeyXO
bech32ToPubKeyXO txt = fromBech32 "npub" txt >>= importPubKeyXO


-- | Convert an Event to naddr bech32 encoding
eventToNaddr :: Event -> T.Text
eventToNaddr event = toBech32 "naddr" $ encodeTLV
  [ (0, BSS.toShort $ getEventId $ eventId event)
  , (1, BSS.toShort $ exportPubKeyXO $ pubKey event)
  , (2, BSS.toShort $ encodeUtf8 $ T.pack $ show $ kind event)
  ]


-- | Decode naddr bech32 encoding to Event components
naddrToEvent :: T.Text -> Maybe (EventId, PubKeyXO, Kind)
naddrToEvent txt = do
  bs <- fromBech32 "naddr" txt
  let tlvs = decodeTLV bs
  eventId' <- lookup 0 tlvs >>= (Just . EventId . BSS.fromShort)
  pubKey' <- lookup 1 tlvs >>= (importPubKeyXO . BSS.fromShort)
  kind' <- lookup 2 tlvs >>= (readMaybe . T.unpack . decodeUtf8 . BSS.fromShort)
  return (eventId', pubKey', kind')


-- | Convert an EventId to note bech32 encoding
eventIdToNote :: EventId -> T.Text
eventIdToNote eid = toBech32 "note" (getEventId eid)


-- | Decode note bech32 encoding to EventId
noteToEventId :: T.Text -> Maybe EventId
noteToEventId txt = do
    bs <- fromBech32 "note" txt
    Just (EventId bs)


-- | Convert an Event to nevent bech32 encoding
eventToNevent :: Event -> [RelayURI] -> T.Text
eventToNevent event relays = toBech32 "nevent" $ encodeTLV tlvData
  where
    tlvData =
      [ (0, BSS.toShort $ getEventId $ eventId event)
      ] ++ map (\r -> (1, BSS.toShort $ encodeUtf8 r)) relays ++
      [ (2, BSS.toShort $ exportPubKeyXO $ pubKey event)
      , (3, BSS.toShort $ LBS.toStrict $ runPut $ putWord32be $ fromIntegral $ kindToInt $ kind event)
      ]


-- | Decode nevent bech32 encoding to Event components
neventToEvent :: T.Text -> Maybe (EventId, [RelayURI], Maybe PubKeyXO, Maybe Kind)
neventToEvent txt = do
  bs <- fromBech32 "nevent" txt
  let tlvs = decodeTLV bs
  eventId' <- lookup 0 tlvs >>= (Just . EventId . BSS.fromShort)
  let relays = mapMaybe (\(t, v) -> if t == 1 then Just (decodeUtf8 $ BSS.fromShort v) else Nothing) tlvs
  let pubKey' = lookup 2 tlvs >>= (importPubKeyXO . BSS.fromShort)
  let kind' = lookup 3 tlvs >>= (readMaybe . T.unpack . decodeUtf8 . BSS.fromShort)
  return (eventId', relays, pubKey', kind')


-- | Convert a PubKeyXO and list of relays to nprofile bech32 encoding
pubKeyXOToNprofile :: PubKeyXO -> [RelayURI] -> T.Text
pubKeyXOToNprofile pubKey' relays = toBech32 "nprofile" $ encodeTLV $
  (0, BSS.toShort $ exportPubKeyXO pubKey') : map (\r -> (1, BSS.toShort $ encodeUtf8 r)) relays


-- | Decode nprofile bech32 encoding to PubKeyXO and relays
nprofileToPubKeyXO :: T.Text -> Maybe (PubKeyXO, [RelayURI])
nprofileToPubKeyXO txt = do
  bs <- fromBech32 "nprofile" txt
  let tlvs = decodeTLV bs
  pubKey' <- case lookup 0 tlvs of
               Just pubKeyBS -> importPubKeyXO (BSS.fromShort pubKeyBS)
               Nothing -> Nothing
  let relays = mapMaybe (\(t, v) ->
                 if t == 1
                 then pure $ decodeUtf8 $ BSS.fromShort v
                 else Nothing) tlvs
  return (pubKey', relays)


-- | Convert a relay URI to nrelay bech32 encoding
relayToNrelay :: RelayURI -> T.Text
relayToNrelay relay = toBech32 "nrelay" $ encodeTLV [(0, BSS.toShort $ encodeUtf8 relay)]


-- | Decode nrelay bech32 encoding to RelayURI
nrelayToRelay :: T.Text -> Maybe RelayURI
nrelayToRelay txt = do
  bs <- fromBech32 "nrelay" txt
  let tlvs = decodeTLV bs
  relayText <- lookup 0 tlvs >>= (Just . decodeUtf8 . BSS.fromShort)
  pure relayText


-- | Convert from bech32 to ByteString
fromBech32 :: T.Text -> T.Text -> Maybe BS.ByteString
fromBech32 hrpText txt = do
    case Bech32.decodeLenient txt of
        Left _ -> Nothing
        Right (prefix, dataPart) ->
            if Bech32.humanReadablePartToText prefix == hrpText
                then Bech32.dataPartToBytes dataPart
                else Nothing

-- | Convert from ByteString to bech32
toBech32 :: T.Text -> BS.ByteString -> T.Text
toBech32 hrpText bs =
    case Bech32.humanReadablePartFromText hrpText of
        Left err -> error $ "Invalid HRP: " ++ show err
        Right hrp -> Bech32.encodeLenient hrp (Bech32.dataPartFromBytes bs)


-- | Encode a list of TLV (Type-Length-Value) items
encodeTLV :: [(Word8, BSS.ShortByteString)] -> BS.ByteString
encodeTLV items = LBS.toStrict $ runPut $ mapM_ encodeTLVItem items
  where
    encodeTLVItem (t, v) = do
      putWord8 t
      putWord8 (fromIntegral $ BSS.length v)
      putByteString (BSS.fromShort v)


-- | Decode a ByteString into a list of TLV (Type-Length-Value) items
decodeTLV :: BS.ByteString -> [(Word8, BSS.ShortByteString)]
decodeTLV bs = runGet go (LBS.fromStrict bs)
  where
    go = do
      empty <- isEmpty
      if empty
        then return []
        else do
          t <- getWord8
          l <- getWord8
          v <- getByteString (fromIntegral l)
          rest <- go
          return $ (t, BSS.toShort v) : rest


-- | Bech32 decoding to EventId
bech32ToEventId :: T.Text -> Maybe EventId
bech32ToEventId txt = do
    case T.take 4 txt of
        "note" -> fromBech32 "note" txt >>= (Just . EventId)
        "nevent" -> do
            bs <- fromBech32 "nevent" txt
            let tlvs = decodeTLV bs
            lookup 0 tlvs >>= (Just . EventId . BSS.fromShort)
        _ -> Nothing
