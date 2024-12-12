{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Nostr.GiftWrap where

import Control.Monad (forM_)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get)
import Effectful.TH (makeEffect)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

import Logging
import Nostr
import Nostr.Event (validateEvent)
import Nostr.Keys (KeyPair, PubKeyXO, byteStringToHex, keyPairToPubKeyXO)
import Nostr.Types (Event(..), EventId(..), Kind(..), Rumor(..), Tag(..))
import Store.Lmdb (LmdbStore, addChatTimelineEntryTx, putEventTx, withTransaction)
import Types (AppState(..), EventWithRelays(..))

-- | GiftWrap Effects.
data GiftWrap :: Effect where
  HandleGiftWrapEvent :: Event -> GiftWrap m ()

-- | Dispatch type for GiftWrap effect.
type instance DispatchOf GiftWrap = Dynamic


makeEffect ''GiftWrap


-- | Effectful type for GiftWrap.
type GiftWrapEff es = ( State AppState :> es
                      , LmdbStore :> es
                      , Nostr :> es
                      , Logging :> es   
                      , IOE :> es
                      )


-- Main effect handler
runGiftWrap :: GiftWrapEff es => Eff (GiftWrap : es) a -> Eff es a
runGiftWrap = interpret $ \_ -> \case
  HandleGiftWrapEvent event' -> do
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
      forM_ chatKey $ \participant -> do
          withTransaction $ \txn -> do
              putEventTx txn $ EventWithRelays originalEvent Set.empty -- @todo add relays where we have seen this event
              addChatTimelineEntryTx txn participant (eventId originalEvent) (rumorCreatedAt decryptedRumor)


-- | Get all p tags from the rumor tags
getAllPTags :: [Tag] -> [PubKeyXO]
getAllPTags = mapMaybe extractPubKey
  where
    extractPubKey (PTag pk _ _) = Just pk
    extractPubKey _ = Nothing
