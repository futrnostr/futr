module Nostr.EventHandler where

import Control.Monad (when)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Data.List (sort)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (atomically, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets)
import Prelude hiding (until)
import Text.Read (readMaybe)

import KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import Logging
import Nostr (Nostr, unwrapGiftWrap, unwrapSeal)
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event ( Event(..), Kind(..), Rumor(..), eventIdFromHex
                   , getAllPubKeysFromPTags, isComment, validateEvent )
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Relay (RelayURI)
import Nostr.Util
import QtQuick
import Store.Lmdb
import Types (AppState(..), RelayPool(..))

-- | Event handler effects
data EventHandler :: Effect where
    HandleEvent :: Maybe RelayURI -> Event -> EventHandler m ()

type instance DispatchOf EventHandler = Dynamic


handleEvent :: EventHandler :> es => Maybe RelayURI -> Event -> Eff es ()
handleEvent mUri event = send $ HandleEvent mUri event


-- | EventHandlerEff
type EventHandlerEff es =
  ( LmdbStore :> es
  , KeyMgmt :> es
  , Nostr :> es
  , State RelayPool :> es
  , State AppState :> es
  , State QtQuickState :> es
  , Util :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  , IOE :> es
  )

-- | Handler for event effects.
runEventHandler
  :: EventHandlerEff es
  => Eff (EventHandler : es) a
  -> Eff es a
runEventHandler = interpret $ \_ -> \case
    HandleEvent mr event' -> do
        updates <- if not (validateEvent event')
            then do
                --logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))
                pure emptyUpdates
            else do
                --logDebug $ "Seen event: " <> pack (show $ kind event') <> " " <> pack (show $ eventId event') <> " on relay: " <> r
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
                                                        else filter (/= keyPairToPubKeyXO kp)
                                                             (rumorPubKey decryptedRumor : sort pListPks)
                                                return $ DecryptedGiftWrapEvent event' DecryptedGiftWrapData
                                                    { participants = participants
                                                    , rumorTimestamp = rumorCreatedAt decryptedRumor
                                                    }
                                            _ -> return $ RawEvent event'
                                    _ -> return $ RawEvent event'
                            _ -> return $ RawEvent event'
                    _ -> return $ RawEvent event'

                PutEventResult{eventIsNew} <- putEvent eventInput mr

                if not eventIsNew then
                    pure emptyUpdates
                else case eventInput of
                    DecryptedGiftWrapEvent _ decrypted -> do
                        st' <- get @AppState
                        case currentProfile st' of
                            Just (pk, _) ->
                                if pk `elem` participants decrypted then
                                    pure $ emptyUpdates { feedChanged = True }
                                else
                                    pure emptyUpdates
                            Nothing ->
                                pure emptyUpdates

                    RawEvent ev -> case kind ev of
                        ShortTextNote -> do
                            if isComment ev then do
                                let parentIds = [ eid
                                                | ("e":eidHex:_) <- tags ev
                                                , Just eid <- [eventIdFromHex eidHex]
                                                ]

                                st' <- get @AppState
                                case currentPost st' of
                                    Just currentPostId ->
                                        if currentPostId `elem` parentIds then
                                            pure $ emptyUpdates { commentFeedChanged = True }
                                        else
                                            pure $ emptyUpdates
                                    Nothing ->
                                        pure $ emptyUpdates
                            else do
                                st' <- get @AppState
                                case currentProfile st' of
                                    Just (pk, _) -> do
                                        if pk == pubKey ev then
                                            pure $ emptyUpdates { feedChanged = True }
                                        else
                                            pure $ emptyUpdates
                                    Nothing -> do
                                        pure $ emptyUpdates

                        Repost -> do
                            st' <- get @AppState
                            case currentProfile st' of
                                Just (pk, _) -> do
                                    if pk == pubKey ev then
                                        pure $ emptyUpdates { feedChanged = True }
                                    else
                                        pure $ emptyUpdates
                                Nothing -> do
                                    pure $ emptyUpdates

                        EventDeletion -> do
                            let kTags = [k | ("k":kStr:_) <- tags ev
                                            , Just k <- [readMaybe (unpack kStr) :: Maybe Int]]

                            if any (== 1059) kTags then
                                -- GiftWrap (1059) - always trigger update for encrypted messages
                                pure $ emptyUpdates { feedChanged = True }
                            else do
                                st' <- get @AppState
                                let feedUpdate = case currentProfile st' of
                                        Just (pk', _) -> 
                                            if pk' == pubKey ev && any (\k -> k == 1 || k == 6) kTags then
                                                 emptyUpdates { feedChanged = True }
                                            else
                                                emptyUpdates
                                        Nothing -> emptyUpdates

                                -- Check if comments for current post are being deleted
                                let commentUpdate = case currentPost st' of
                                        Just _ ->
                                            if any (== 1) kTags then  -- ShortTextNote (1) could be comments
                                                emptyUpdates { commentFeedChanged = True }
                                            else
                                                emptyUpdates
                                        Nothing -> emptyUpdates

                                pure $ feedUpdate <> commentUpdate

                        Metadata -> do
                            case eitherDecode (fromStrict $ encodeUtf8 $ content ev) of
                                Right profile -> do
                                    kp <- getKeyPair
                                    let isOwnProfile = pubKey ev == keyPairToPubKeyXO kp

                                    -- update account profile
                                    when isOwnProfile $ do
                                        let aid = AccountId $ pubKeyXOToBech32 (pubKey ev)
                                        updateProfile aid profile

                                    -- Defer expensive Qt operations to batching system - just record the pubkey
                                    pure $ emptyUpdates { profilePubkeysToUpdate = [pubKey ev] }

                                Left _ -> pure emptyUpdates

                        FollowList -> do
                            kp <- getKeyPair
                            let pk = keyPairToPubKeyXO kp
                            pure $ emptyUpdates { myFollowsChanged = pk == pubKey ev }

                        GiftWrap -> do
                            kp <- getKeyPair
                            mSealedEvent <- unwrapGiftWrap ev kp
                            case mSealedEvent of
                                Just sealedEvent | validateEvent sealedEvent ->
                                    case kind sealedEvent of
                                        Seal -> do
                                            mDecryptedRumor <- unwrapSeal sealedEvent kp
                                            case mDecryptedRumor of
                                                Just decryptedRumor
                                                    | pubKey sealedEvent == rumorPubKey decryptedRumor -> do
                                                        let tags' = rumorTags decryptedRumor
                                                            pListPks = getAllPubKeysFromPTags tags'

                                                        st' <- get @AppState
                                                        case currentProfile st' of
                                                            Just (pk, _) -> pure $ emptyUpdates { feedChanged = pk `elem` pListPks }
                                                            Nothing -> pure emptyUpdates
                                                _ -> pure $ emptyUpdates
                                        _ -> pure $ emptyUpdates
                                _ -> pure $ emptyUpdates

                        RelayListMetadata -> do
                            kp <- getKeyPair
                            let pk = keyPairToPubKeyXO kp
                            let isOwnEvent = pk == pubKey ev
                            pure $ emptyUpdates { generalRelaysChanged = isOwnEvent }

                        PreferredDMRelays -> do
                            kp <- getKeyPair
                            let pk = keyPairToPubKeyXO kp
                            pure $ emptyUpdates { dmRelaysChanged = pk == pubKey ev }

                        _ -> do
                            --logDebug $ "Ignoring event of kind: " <> pack (show (kind ev))
                            pure $ emptyUpdates

        when (myFollowsChanged updates || dmRelaysChanged updates || generalRelaysChanged updates) $ do
            q <- gets @RelayPool updateQueue
            atomically $ writeTQueue q ()

        --logDebug $ "Event: " <> pack (show event')
        --logDebug $ "Notifying updates: " <> pack (show updates)
        when (hasUpdates updates) $ do
            notify updates
