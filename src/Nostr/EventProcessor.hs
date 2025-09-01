module Nostr.EventProcessor (handleEvent) where

import Control.Monad (unless, when)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Data.List (sort)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.State.Static.Shared (State, get)
import Text.Read (readMaybe)

import KeyMgmt (KeyMgmt, updateProfile, AccountId(..))
import Nostr (Nostr, unwrapGiftWrap, unwrapSeal)
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event ( Event(..), Kind(..), Rumor(..), eventIdFromHex
                   , getAllPubKeysFromPTags, isComment, validateEvent )
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Util (Util, getKeyPair)
import Nostr.Types (RelayURI)
import QtQuick (QtQuick, UIUpdates(..), emptyUpdates, hasUpdates, notify)
import Store.Lmdb ( DecryptedGiftWrapData(..), LmdbStore, PutEventInput(..), putEvent)
import Types (AppState(..))


type EventProcessingEff es =
  ( LmdbStore :> es
  , KeyMgmt :> es
  , Nostr :> es
  , Util :> es
  , State AppState :> es
  , QtQuick :> es
  )


handleEvent :: EventProcessingEff es => Event -> RelayURI -> Eff es ()
handleEvent event' relayUri =
  if not (validateEvent event')
    then pure ()
    else do
      eventInput <- case kind event' of
        GiftWrap -> do
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
                          participants = if rumorPubKey decryptedRumor == keyPairToPubKeyXO kp
                            then sort pListPks
                            else filter (/= keyPairToPubKeyXO kp) (rumorPubKey decryptedRumor : sort pListPks)
                      pure $ DecryptedGiftWrapEvent event' DecryptedGiftWrapData
                        { participants = participants
                        , rumorTimestamp = rumorCreatedAt decryptedRumor
                        }
                    _ -> pure $ RawEvent event'
                _ -> pure $ RawEvent event'
            _ -> pure $ RawEvent event'
        _ -> pure $ RawEvent event'

      eventIsNew <- putEvent eventInput (Just relayUri)

      when eventIsNew $ case eventInput of
        DecryptedGiftWrapEvent _ decrypted -> do
          st' <- get @AppState
          case currentProfile st' of
            Just (pk, _) -> when (pk `elem` participants decrypted) $
              notify $ emptyUpdates { feedChanged = True }
            Nothing -> pure ()

        RawEvent ev -> case kind ev of
          ShortTextNote -> do
            if isComment ev then do
              let parentIds = [ eid
                              | ("e":eidHex:_) <- tags ev
                              , Just eid <- [eventIdFromHex eidHex]
                              ]
              st' <- get @AppState
              case currentPost st' of
                Just currentPostId -> when (currentPostId `elem` parentIds) $
                  notify $ emptyUpdates { commentFeedChanged = True }
                Nothing -> pure ()
            else do
              st' <- get @AppState
              case currentProfile st' of
                Just (pk, _) -> when (pk == pubKey ev) $
                  notify $ emptyUpdates { feedChanged = True }
                Nothing -> pure ()

          Repost -> do
            st' <- get @AppState
            case currentProfile st' of
              Just (pk, _) -> when (pk == pubKey ev) $
                notify $ emptyUpdates { feedChanged = True }
              Nothing -> pure ()

          EventDeletion -> do
            let kTags = [k | ("k":kStr:_) <- tags ev
                           , Just k <- [readMaybe (unpack kStr) :: Maybe Int]]
            if any (== 1059) kTags then
              notify $ emptyUpdates { feedChanged = True }
            else do
              st' <- get @AppState
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
            case eitherDecode (fromStrict $ encodeUtf8 $ content ev) of
              Right profile -> do
                kp <- getKeyPair
                let isOwnProfile = pubKey ev == keyPairToPubKeyXO kp
                when isOwnProfile $ do
                  let aid = AccountId $ pubKeyXOToBech32 (pubKey ev)
                  updateProfile aid profile
                notify $ emptyUpdates { profilePubkeysToUpdate = [pubKey ev] }
              Left _ -> pure ()

          FollowList -> do
            kp <- getKeyPair
            let pk = keyPairToPubKeyXO kp
            notify $ emptyUpdates { myFollowsChanged = pk == pubKey ev }

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
                        st' <- get @AppState
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
            notify $ emptyUpdates { generalRelaysChanged = pk == pubKey ev }

          PreferredDMRelays -> do
            kp <- getKeyPair
            let pk = keyPairToPubKeyXO kp
            notify $ emptyUpdates { dmRelaysChanged = pk == pubKey ev }

          _ -> pure ()


