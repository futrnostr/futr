module Nostr.EventProcessor (handleEvent) where

import Control.Monad (unless, when)
import Data.Aeson (eitherDecodeStrict')
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Read (decimal)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.State.Static.Shared (State, get, modify)

import KeyMgmt (KeyMgmt, updateProfile, AccountId(..))
import Logging
import Nostr (Nostr, unwrapGiftWrap, unwrapSeal)
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event ( Event(..), Kind(..), Rumor(..), eventIdFromHex
                   , getAllPubKeysFromPTags, isComment, validateEvent )
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Util (Util, getKeyPair)
import Nostr.Types (RelayURI, getUri)
import QtQuick (QtQuick, UIUpdates(..), emptyUpdates, hasUpdates, notify)
import Store.Lmdb ( DecryptedGiftWrapData(..), LmdbStore, PutEventInput(..)
                  , putEvent, getFollows, getGeneralRelays, getDMRelays )
import Types (AppState(..), Follow(..))


type EventProcessingEff es =
  ( LmdbStore :> es
  , Logging :> es
  , KeyMgmt :> es
  , Nostr :> es
  , Util :> es
  , State AppState :> es
  , QtQuick :> es
  , Concurrent :> es
  )


-- | Handle an event
handleEvent :: EventProcessingEff es => Event -> RelayURI -> Eff es ()
handleEvent event' relayUri =
  -- Only validate event ID, defer expensive signature verification
  if not (validateEvent event')
    then pure ()
    else do
      -- Process event input more efficiently, avoiding unnecessary allocations
      eventInput <- case kind event' of
        GiftWrap -> processGiftWrapEvent event'
        _ -> pure $ RawEvent event'

      eventIsNew <- putEvent eventInput (Just relayUri)

      when eventIsNew $ do
        -- Get app state once to avoid repeated lookups
        st' <- get @AppState
        case eventInput of
          DecryptedGiftWrapEvent _ decrypted -> do
            case currentProfile st' of
              Just (pk, _) -> when (pk `elem` participants decrypted) $
                notify $ emptyUpdates { feedChanged = True }
              Nothing -> pure ()

          RawEvent ev -> processRawEvent ev st'


-- | Process GiftWrap events
processGiftWrapEvent :: EventProcessingEff es => Event -> Eff es PutEventInput
processGiftWrapEvent event' = do
  kp <- getKeyPair
  mSealedEvent <- unwrapGiftWrap event' kp
  case mSealedEvent of
    Just sealedEvent | validateEvent sealedEvent ->
      case kind sealedEvent of
        Seal -> do
          mDecryptedRumor <- unwrapSeal sealedEvent kp
          case mDecryptedRumor of
            Just decryptedRumor | pubKey sealedEvent == rumorPubKey decryptedRumor -> do
              let tags' = rumorTags decryptedRumor
                  pListPks = getAllPubKeysFromPTags tags'
                  -- Pre-compute the key pair to avoid repeated calls
                  kpPubKey = keyPairToPubKeyXO kp
                  participants = if rumorPubKey decryptedRumor == kpPubKey
                    then sort pListPks
                    else filter (/= kpPubKey) (rumorPubKey decryptedRumor : sort pListPks)
              pure $ DecryptedGiftWrapEvent event' DecryptedGiftWrapData
                { participants = participants
                , rumorTimestamp = rumorCreatedAt decryptedRumor
                }
            _ -> pure $ RawEvent event'
        _ -> pure $ RawEvent event'
    _ -> pure $ RawEvent event'


-- | Process raw events
processRawEvent :: EventProcessingEff es => Event -> AppState -> Eff es ()
processRawEvent ev st' = case kind ev of
  ShortTextNote -> do
    if isComment ev then do
      let parentIds = [ eid
                      | ("e":eidHex:_) <- tags ev
                      , Just eid <- [eventIdFromHex eidHex]
                      ]
      case currentPost st' of
        Just currentPostId -> when (currentPostId `elem` parentIds) $
          notify $ emptyUpdates { commentFeedChanged = True }
        Nothing -> pure ()
    else do
      case currentProfile st' of
        Just (pk, _) -> when (pk == pubKey ev) $
          notify $ emptyUpdates { feedChanged = True }
        Nothing -> pure ()

  Repost -> do
    case currentProfile st' of
      Just (pk, _) -> when (pk == pubKey ev) $
        notify $ emptyUpdates { feedChanged = True }
      Nothing -> pure ()

  EventDeletion -> do
    let kTags = [k | ("k":kStr:_) <- tags ev
                   , Right (k, "") <- [decimal kStr :: Either String (Int, Text)]]
    if any (== 1059) kTags then
      notify $ emptyUpdates { feedChanged = True }
    else do
      let feedUpdate = case currentProfile st' of
            Just (pk', _) -> if pk' == pubKey ev && any (\k -> k == 1 || k == 6) kTags
                              then emptyUpdates { feedChanged = True }
                              else emptyUpdates
            Nothing -> emptyUpdates
      let commentUpdate = case currentPost st' of
            Just _ -> if any (== 1) kTags
                      then emptyUpdates { commentFeedChanged = True }
                      else emptyUpdates
            Nothing -> emptyUpdates
      unless (not $ hasUpdates (feedUpdate <> commentUpdate)) $
        notify (feedUpdate <> commentUpdate)

  Metadata -> do
    case eitherDecodeStrict' (encodeUtf8 $ content ev) of
      Right profile -> do
        kp <- getKeyPair
        let isOwnProfile = pubKey ev == keyPairToPubKeyXO kp
        when isOwnProfile $ do
          let aid = AccountId $ pubKeyXOToBech32 (pubKey ev)
          updateProfile aid profile
        notify $ emptyUpdates { profilePubkeysToUpdate = [pubKey ev] }
      Left _ -> pure ()

  FollowList -> do
    xo <- keyPairToPubKeyXO <$> getKeyPair
    when (xo == pubKey ev) $ do
      follows <- getFollows xo
      modify @AppState $ \s -> s { currentFollows = Map.fromList $ zip (map pubkey follows) follows }    
    notify $ emptyUpdates { myFollowsChanged = xo == pubKey ev }

  GiftWrap -> do
    kp <- getKeyPair
    mSealedEvent <- unwrapGiftWrap ev kp
    case mSealedEvent of
      Just sealedEvent | validateEvent sealedEvent ->
        case kind sealedEvent of
          Seal -> do
            mDecryptedRumor <- unwrapSeal sealedEvent kp
            case mDecryptedRumor of
              Just decryptedRumor | pubKey sealedEvent == rumorPubKey decryptedRumor -> do
                let tags' = rumorTags decryptedRumor
                    pListPks = getAllPubKeysFromPTags tags'
                case currentProfile st' of
                  Just (pk, _) -> when (pk `elem` pListPks) $
                    notify $ emptyUpdates { feedChanged = True }
                  Nothing -> pure ()
              _ -> pure ()
          _ -> pure ()
      _ -> pure ()

  RelayListMetadata -> do
    kp <- getKeyPair
    let pk = keyPairToPubKeyXO kp
    when (pk == pubKey ev) $ do
      generalRelays <- getGeneralRelays pk
      modify @AppState $ \s -> s { currentGeneralRelays = Map.fromList $ zip (map getUri generalRelays) generalRelays }
    notify $ emptyUpdates { generalRelaysChanged = pk == pubKey ev }

  PreferredDMRelays -> do
    kp <- getKeyPair
    let pk = keyPairToPubKeyXO kp
    when (pk == pubKey ev) $ do
      dmRelays <- getDMRelays pk
      modify @AppState $ \s -> s { currentDMRelays = Map.fromList $ zip dmRelays (repeat ()) }
    notify $ emptyUpdates { dmRelaysChanged = pk == pubKey ev }

  _ -> pure ()
