{-# LANGUAGE NamedFieldPuns #-}

module Nostr.EventHandler where

import Control.Monad (forM, when)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (atomically, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets)
import Graphics.QML qualified as QML
import Prelude hiding (until)
import Text.Read (readMaybe)

import KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import Logging
import Nostr (Nostr, unwrapGiftWrap, unwrapSeal)
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event ( Event(..), EventId(..), Kind(..), Rumor(..), eventIdFromHex
                   , getAllPubKeysFromPTags, isComment, validateEvent )
import Nostr.Keys (byteStringToHex, keyPairToPubKeyXO)
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
                logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))
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

                PutEventResult{eventIsNew, relaysUpdated} <- putEvent eventInput mr

                relayObjRefs <- if relaysUpdated then do
                    pmap <- gets @QtQuickState propertyMap
                    case Map.lookup (eventId event') (postObjRefs pmap) >>= Map.lookup "relays" of
                        Just weakRef -> do
                            objRef <- liftIO $ QML.fromWeakObjRef weakRef
                            pure [objRef]
                        Nothing -> pure []
                else pure []

                if not eventIsNew then
                    pure emptyUpdates { postObjectsToSignal = relayObjRefs }
                else case eventInput of
                    DecryptedGiftWrapEvent _ decrypted -> do
                        st <- get @AppState
                        case currentProfile st of
                            Just (pk, _) ->
                                if pk `elem` participants decrypted then
                                    pure $ emptyUpdates { privateMessagesChanged = True }
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

                                pmap <- gets @QtQuickState propertyMap
                                commentObjRefs <- do
                                    refs <- forM parentIds $ \parentId -> do
                                        case Map.lookup parentId (postObjRefs pmap) >>= Map.lookup "comments" of
                                            Just weakRef -> do
                                                objRef <- liftIO $ QML.fromWeakObjRef weakRef
                                                pure $ Just objRef
                                            Nothing -> pure Nothing
                                    pure $ catMaybes refs

                                pure $ emptyUpdates { postObjectsToSignal = commentObjRefs }
                            else do
                                st <- get @AppState
                                case currentProfile st of
                                    Just (pk, _) -> do
                                        if pk == pubKey ev then
                                            pure $ emptyUpdates { postsChanged = True }
                                        else
                                            pure $ emptyUpdates
                                    Nothing -> do
                                        pure $ emptyUpdates

                        Repost -> do
                            st <- get @AppState
                            case currentProfile st of
                                Just (pk, _) -> do
                                    if pk == pubKey ev then do
                                        pure $ emptyUpdates { postsChanged = True }
                                    else
                                        pure $ emptyUpdates
                                Nothing -> do
                                    pure $ emptyUpdates

                        EventDeletion -> do
                            let kTags = [k | ("k":kStr:_) <- tags ev
                                            , Just k <- [readMaybe (unpack kStr) :: Maybe Int]]

                            if any (== 1059) kTags then
                                -- GiftWrap (1059) - always trigger update for encrypted messages
                                pure $ emptyUpdates { privateMessagesChanged = True }
                            else do
                                st <- get @AppState
                                case currentProfile st of
                                    Just (pk', _) -> do
                                        if pk' == pubKey ev then
                                            if any (\k -> k == 1 || k == 6) kTags then
                                                -- ShortTextNote (1) or Repost (6)
                                                pure $ emptyUpdates { postsChanged = True }
                                            else
                                                pure $ emptyUpdates
                                        else
                                            pure $ emptyUpdates
                                    Nothing ->
                                        pure $ emptyUpdates

                        Metadata -> do
                            case eitherDecode (fromStrict $ encodeUtf8 $ content ev) of
                                Right profile -> do
                                    kp <- getKeyPair
                                    let isOwnProfile = pubKey ev == keyPairToPubKeyXO kp

                                    -- update account profile
                                    when isOwnProfile $ do
                                        let aid = AccountId $ pubKeyXOToBech32 (pubKey ev)
                                        updateProfile aid profile

                                    -- signal profile object changes
                                    pmap <- gets @QtQuickState propertyMap
                                    profileObjRefs <- do
                                        let profileWeakRefs = concatMap Map.elems $ Map.lookup (pubKey ev) $ profileObjRefs pmap
                                        refs <- forM profileWeakRefs $ \weakRef ->
                                            liftIO $ QML.fromWeakObjRef weakRef
                                        pure refs

                                    -- Get content refs that reference this profile
                                    contentRefs <- do
                                        let contentWeakRefs = fromMaybe [] $ Map.lookup (pubKey ev) $ profileContentRefs pmap
                                        forM contentWeakRefs $ \weakRef -> do
                                            liftIO $ QML.fromWeakObjRef weakRef

                                    pure $ emptyUpdates
                                        { profileObjectsToSignal = profileObjRefs
                                        , contentObjectsToSignal = contentRefs }

                                Left err -> do
                                    logWarning $ "Failed to decode metadata: " <> pack err
                                    pure emptyUpdates

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

                                                        st <- get @AppState
                                                        case currentProfile st of
                                                            Just (pk, _) -> do
                                                                if pk `elem` pListPks then do
                                                                    pure $ emptyUpdates { privateMessagesChanged = True }
                                                                else
                                                                    pure $ emptyUpdates
                                                            Nothing -> do
                                                                pure $ emptyUpdates
                                                _ -> pure $ emptyUpdates
                                        _ -> pure $ emptyUpdates
                                _ -> pure $ emptyUpdates

                        RelayListMetadata -> do
                            kp <- getKeyPair
                            let pk = keyPairToPubKeyXO kp
                            pure $ emptyUpdates { generalRelaysChanged = pk == pubKey ev }

                        PreferredDMRelays -> do
                            kp <- getKeyPair
                            let pk = keyPairToPubKeyXO kp
                            pure $ emptyUpdates { dmRelaysChanged = pk == pubKey ev }

                        _ -> do
                            logDebug $ "Ignoring event of kind: " <> pack (show (kind ev))
                            pure $ emptyUpdates

        when (myFollowsChanged updates || dmRelaysChanged updates || generalRelaysChanged updates) $ do
            q <- gets @RelayPool updateQueue
            atomically $ writeTQueue q ()

        --logDebug $ "Event: " <> pack (show event')
        --logDebug $ "Notifying updates: " <> pack (show updates)
        when (hasUpdates updates) $ do
            notify updates
