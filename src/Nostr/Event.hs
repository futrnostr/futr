-- | Module: Nostr.Event
-- Defines functions related to events in the Nostr protocol.

{-# LANGUAGE RecordWildCards     #-}

module Nostr.Event where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.URI (emptyURI)

import Nostr.Keys
import Nostr.Types


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

validateEventId :: Event -> Bool
validateEventId e = 
  let serializedEvent = toStrict $ encode $ UnsignedEvent
        { pubKey' = pubKey e
        , createdAt' = createdAt e
        , kind' = kind e
        , tags' = tags e
        , content' = content e
        }
  in (getEventId $ eventId e) == SHA256.hash serializedEvent

verifySignature :: Event -> Bool -- @todo: implement delagate verification (subkeys?)
verifySignature e = schnorrVerify (pubKey e) (getEventId $ eventId e) (sig e)

shortTextNote :: Text -> PubKeyXO -> Int -> UnsignedEvent
shortTextNote note xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = ShortTextNote
    , tags' = []
    , content' = note
    }

setMetadata :: Profile -> PubKeyXO -> Int -> UnsignedEvent
setMetadata p xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = Metadata
    , tags' = []
    , content' = decodeUtf8 $ toStrict $ encode p
    }

readProfile :: Event -> Maybe Profile
readProfile event = case kind event of
  Metadata ->
    decode $ fromStrict $ encodeUtf8 $ content event
  _ ->
    Nothing

replyNote :: Event -> Text -> PubKeyXO -> Int -> UnsignedEvent
replyNote event note xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = ShortTextNote
    , tags' = [ETag (eventId event) Nothing (Just Reply)]
    , content' = note
    }

setContacts :: [(PubKeyXO, Maybe DisplayName)] -> PubKeyXO -> Int -> UnsignedEvent
setContacts contacts xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = FollowList
    , tags' = map (\c -> PTag (fst c) (Just (RelayURI emptyURI)) (snd c)) contacts
    , content' = ""
    }

deleteEvents :: [EventId] -> Text -> PubKeyXO -> Int -> UnsignedEvent
deleteEvents eids reason xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
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
