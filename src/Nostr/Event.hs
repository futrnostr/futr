-- | Module: Nostr.Event
-- Defines functions related to events in the Nostr protocol.

{-# LANGUAGE RecordWildCards     #-}

module Nostr.Event where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


import Nostr.Keys
import Nostr.Types
import Nostr.Encryption (decrypt, getConversationKey, encrypt)
import Data.Time.Clock.POSIX (getCurrentTime, utcTimeToPOSIXSeconds)
import Crypto.Random (getRandomBytes)
import System.Random (randomRIO)


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


-- | Create a reply note event.
createReplyNote :: Event -> Text -> PubKeyXO -> Int -> UnsignedEvent
createReplyNote event note xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = ShortTextNote
    , tags' = [ETag (eventId event) Nothing (Just Reply)]
    , content' = note
    }


-- | Create a follow list event.
createFollowList :: [(PubKeyXO, Maybe DisplayName)] -> PubKeyXO -> Int -> UnsignedEvent
createFollowList contacts xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = FollowList
    , tags' = map (\c -> PTag (fst c) (Just "") (snd c)) contacts
    , content' = ""
    }


-- | Create a delete event.
createEventDeletion :: [EventId] -> Text -> PubKeyXO -> Int -> UnsignedEvent
createEventDeletion eids reason xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = EventDeletion
    , tags' = toDelete
    , content' = reason
    }
  where
    toDelete = map (\eid -> ETag eid Nothing Nothing) eids


createRelayListMetadataEvent :: [Relay] -> PubKeyXO -> Int -> UnsignedEvent
createRelayListMetadataEvent relays xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = RelayListMetadata
    , tags' = map (\r -> RelayTag r) relays
    , content' = ""
    }


createPreferredDMRelaysEvent :: [RelayURI] -> PubKeyXO -> Int -> UnsignedEvent
createPreferredDMRelaysEvent urls xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = PreferredDMRelays
    , tags' = map (\url -> RelayTag $ InboxOutboxRelay url) urls
    , content' = ""
    }


createCanonicalAuthentication :: RelayURI -> Text -> PubKeyXO -> Int -> UnsignedEvent
createCanonicalAuthentication r challenge xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = CanonicalAuthentication
    -- force the relay to be a InboxOutboxRelay for the purpose of authentication
    , tags' = [RelayTag $ InboxOutboxRelay r, ChallengeTag challenge]
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
                , tags' = [PTag recipientPubKey Nothing Nothing]
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


-- | Read profile from event.
readProfile :: Event -> Maybe Profile
readProfile event = case kind event of
  Metadata ->
    decode $ fromStrict $ encodeUtf8 $ content event
  _ ->
    Nothing


-- | Get the reply event ID.
getReplyEventId :: Event -> Maybe EventId
getReplyEventId = getRelationshipEventId Reply


-- | Get the root event ID.
getRootEventId :: Event -> Maybe EventId
getRootEventId = getRelationshipEventId Root


-- | Get the relationship event ID.
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
