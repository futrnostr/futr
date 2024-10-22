{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Nostr.GiftWrap where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH (makeEffect)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Logging
import Nostr
import Nostr.Event (validateEvent)
import Nostr.Keys (KeyPair, PubKeyXO, byteStringToHex, keyPairToPubKeyXO)
import Nostr.Types (Event(..), EventId(..), Kind(..), Rumor(..), Tag(..))
import Types (AppState(..),ChatMessage(..))

-- | GiftWrap Effects.
data GiftWrap :: Effect where
  HandleGiftWrapEvent :: Event -> GiftWrap m ()

-- | Dispatch type for GiftWrap effect.
type instance DispatchOf GiftWrap = Dynamic


makeEffect ''GiftWrap


-- | Effectful type for GiftWrap.
type GiftWrapEff es = ( State AppState :> es
                      , Logging :> es   
                      , IOE :> es
                      , Nostr :> es
                      )


-- Main effect handler
runGiftWrap :: GiftWrapEff es => Eff (GiftWrap : es) a -> Eff es a
runGiftWrap = interpret $ \_ -> \case
  HandleGiftWrapEvent event' -> do
    logDebug $ "Handling gift wrap event: " <> pack (show event')
    st <- get @AppState
    case keyPair st of
      Just kp -> do
        unwrappedEvent <- unwrapGiftWrap event' kp
        case unwrappedEvent of
          Just sealedEvent -> handleSealedEvent sealedEvent event' kp
          Nothing -> logInfo "Failed to unwrap gift wrap"
      Nothing -> logWarning "No key pair available to decrypt gift wrap"


-- Helper functions

-- | Handle a sealed event.
handleSealedEvent :: GiftWrapEff es => Event -> Event -> KeyPair -> Eff es ()
handleSealedEvent sealedEvent originalEvent kp
  | not (validateEvent sealedEvent) =
      logWarning $ "Invalid sealed event: " <> (byteStringToHex $ getEventId (eventId originalEvent))
  | kind sealedEvent /= Seal =
      logInfo "Unwrapped event is not a Seal"
  | otherwise = do
      unwrappedRumor <- unwrapSeal sealedEvent kp
      case unwrappedRumor of
        Just decryptedRumor -> processDecryptedRumor decryptedRumor sealedEvent originalEvent kp
        Nothing -> logInfo "Failed to decrypt sealed event"


-- | Process a decrypted rumor.
processDecryptedRumor :: GiftWrapEff es => Rumor -> Event -> Event -> KeyPair -> Eff es ()
processDecryptedRumor decryptedRumor sealedEvent originalEvent kp
  | not (pubKey sealedEvent == rumorPubKey decryptedRumor) =
      logWarning $ "Rumor pubkey does not match sealed event pubkey: " <> (byteStringToHex $ getEventId (eventId originalEvent))
  | otherwise = do
      let chatKey = if rumorPubKey decryptedRumor == keyPairToPubKeyXO kp
            then sort $ getAllPTags (rumorTags decryptedRumor)
            else filter (/= keyPairToPubKeyXO kp) $ rumorPubKey decryptedRumor : sort (getAllPTags (rumorTags decryptedRumor))
      let senderPubKey = rumorPubKey decryptedRumor
      let chatMsg = createChatMessage originalEvent decryptedRumor senderPubKey
      updateChats chatKey chatMsg


-- | Update chats.
updateChats :: State AppState :> es => [PubKeyXO] -> ChatMessage -> Eff es ()
updateChats chatKey chatMsg = do
  modify $ \s -> s { chats = mergeMessageIntoChats chatKey chatMsg (chats s) }


-- | Get all p tags from the rumor tags
getAllPTags :: [Tag] -> [PubKeyXO]
getAllPTags = mapMaybe extractPubKey
  where
    extractPubKey (PTag pk _ _) = Just pk
    extractPubKey _ = Nothing


-- | Create a chat message.
createChatMessage :: Event -> Rumor -> PubKeyXO -> ChatMessage
createChatMessage originalEvent decryptedRumor senderPubKey =
  ChatMessage
    { chatMessageId = eventId originalEvent
    , chatMessage = rumorContent decryptedRumor
    , author = senderPubKey
    , chatMessageCreatedAt = rumorCreatedAt decryptedRumor
    , timestamp = pack $ formatTime defaultTimeLocale "%FT%T%QZ" $ posixSecondsToUTCTime $ fromIntegral $ rumorCreatedAt decryptedRumor
    }


-- | Merge a new chat message into the existing chat map
mergeMessageIntoChats :: [PubKeyXO] -> ChatMessage -> Map.Map [PubKeyXO] [ChatMessage] -> Map.Map [PubKeyXO] [ChatMessage]
mergeMessageIntoChats chatKey chatMsg = Map.alter (addOrUpdateChatThread chatMsg) chatKey


-- | Add a new message to an existing chat thread or create a new thread
addOrUpdateChatThread :: ChatMessage -> Maybe [ChatMessage] -> Maybe [ChatMessage]
addOrUpdateChatThread chatMsg = \case
  Just msgs -> Just $ insertUniqueMessage chatMsg msgs
  Nothing -> Just [chatMsg]

-- | Insert a message into a list, ensuring no duplicates
insertUniqueMessage :: ChatMessage -> [ChatMessage] -> [ChatMessage]
insertUniqueMessage newMsg = foldr insertIfUnique [newMsg]
  where
    insertIfUnique msg acc
      | chatMessageId msg == chatMessageId newMsg = acc
      | otherwise = msg : acc
