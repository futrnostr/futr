-- | Module: Nostr.Event
-- Defines functions related to events in the Nostr protocol.

{-# LANGUAGE RecordWildCards     #-}

module Nostr.Event where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
import Data.Aeson.Text (encodeToTextBuilder)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Int (Int64)
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder (toLazyText)
import Text.URI (emptyURI)

import Nostr.Keys
import Nostr.Types

signEvent :: UnsignedEvent -> KeyPair -> Maybe Event
signEvent u kp =
  Event eid (pubKey' u) (createdAt' u) (kind' u) (tags' u) (content' u) <$> schnorrSign kp (getEventId eid)
  where
    eid = EventId { getEventId = SHA256.hash $ toStrict $ encode u }

validateEventId :: Event -> Bool
validateEventId e = (getEventId $ eventId e) == (SHA256.hash $ toStrict $ encode e)

verifySignature :: Event -> Bool -- @todo: implement delagate verification (subkeys?)
verifySignature e = schnorrVerify (pubKey e) (toStrict $ encode e) (sig e)

textNote :: Text -> PubKeyXO -> Int64 -> UnsignedEvent
textNote note xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = ShortTextNote
    , tags' = []
    , content' = note
    }

setMetadata :: Profile -> PubKeyXO -> Int64 -> UnsignedEvent
setMetadata profile xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = Metadata
    , tags' = []
    , content' = LazyText.toStrict . toLazyText . encodeToTextBuilder . toJSON $ profile
    }

readProfile :: Event -> Maybe Profile
readProfile event = case kind event of
  Metadata ->
    decode $ fromStrict $ encodeUtf8 $ content event
  _ ->
    Nothing

replyNote :: Event -> Text -> PubKeyXO -> Int64 -> UnsignedEvent
replyNote event note xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = ShortTextNote
    , tags' = [ETag (eventId event) Nothing (Just Reply)]
    , content' = note
    }

setContacts :: [(PubKeyXO, Maybe DisplayName)] -> PubKeyXO -> Int64 -> UnsignedEvent
setContacts contacts xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = FollowList
    , tags' = map (\c -> PTag (fst c) (Just (RelayURI emptyURI)) (snd c)) contacts
    , content' = ""
    }

deleteEvents :: [EventId] -> Text -> PubKeyXO -> Int64 -> UnsignedEvent
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
