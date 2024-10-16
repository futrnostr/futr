-- | Module: Nostr.Event
-- Defines functions related to events in the Nostr protocol.

{-# LANGUAGE RecordWildCards     #-}

module Nostr.Event where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.URI (emptyURI)

import Nostr.Keys
import Nostr.Types


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
    , tags' = map (\c -> PTag (fst c) (Just (RelayURI emptyURI)) (snd c)) contacts
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
