{-# LANGUAGE NamedFieldPuns #-}

module Nostr.EventHandler where

import Control.Monad (forM_,when)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Data.Map qualified as Map
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (atomically, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets)
import Effectful.TH
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

makeEffect ''EventHandler

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
                PutEventResult{eventIsNew, relaysUpdated} <- putEvent event' mr

                when relaysUpdated $ do
                    pmap <- gets @QtQuickState propertyMap
                    let maybeRef = Map.lookup (eventId event') (postObjRefs pmap) >>= Map.lookup "relays"
                    forM_ maybeRef $ \weakRef -> do
                        objRef <- liftIO $ QML.fromWeakObjRef weakRef
                        signalPost objRef

                if not eventIsNew then
                    pure $ emptyUpdates
                else case kind event' of
                    ShortTextNote -> do
                        if isComment event' then do
                            let parentIds = [ eid
                                            | ("e":eidHex:_) <- tags event'
                                            , Just eid <- [eventIdFromHex eidHex]
                                            ]

                            pmap <- gets @QtQuickState propertyMap
                            forM_ parentIds $ \parentId -> do
                                let maybeRef = Map.lookup parentId (postObjRefs pmap) >>= Map.lookup "comments"
                                forM_ maybeRef $ \weakRef -> do
                                    objRef <- liftIO $ QML.fromWeakObjRef weakRef
                                    signalPost objRef

                            pure $ emptyUpdates
                        else do
                            st <- get @AppState
                            case currentProfile st of
                                Just (pk, _) -> do
                                    if pk == pubKey event' then do
                                        pure $ emptyUpdates { postsChanged = True }
                                    else
                                        pure $ emptyUpdates
                                Nothing -> do
                                    pure $ emptyUpdates

                    Repost -> do
                        st <- get @AppState
                        case currentProfile st of
                            Just (pk, _) -> do
                                if pk == pubKey event' then do
                                    pure $ emptyUpdates { postsChanged = True }
                                else
                                    pure $ emptyUpdates
                            Nothing -> do
                                pure $ emptyUpdates

                    EventDeletion -> do
                        let kTags = [k | ("k":kStr:_) <- tags event'
                                        , Just k <- [readMaybe (unpack kStr) :: Maybe Int]]

                        if any (== 1059) kTags then
                            -- GiftWrap (1059) - always trigger update for encrypted messages
                            pure $ emptyUpdates { privateMessagesChanged = True }
                        else do
                            st <- get @AppState
                            case currentProfile st of
                                Just (pk', _) -> do
                                    if pk' == pubKey event' then
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
                        case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                            Right profile -> do
                                kp <- getKeyPair
                                let isOwnProfile = pubKey event' == keyPairToPubKeyXO kp

                                -- update account profile
                                when isOwnProfile $ do
                                    let aid = AccountId $ pubKeyXOToBech32 (pubKey event')
                                    updateProfile aid profile

                                -- signal profile object changes
                                pmap <- gets @QtQuickState propertyMap
                                let profileWeakRefs = concatMap Map.elems $ Map.lookup (pubKey event') $ profileObjRefs pmap
                                forM_ profileWeakRefs $ \weakRef -> do
                                    objRef <- liftIO $ QML.fromWeakObjRef weakRef
                                    signalProfile objRef

                                pure $ emptyUpdates
                            Left err -> do
                                logWarning $ "Failed to decode metadata: " <> pack err
                                pure emptyUpdates

                    FollowList -> do
                        kp <- getKeyPair
                        let pk = keyPairToPubKeyXO kp
                        pure $ emptyUpdates { myFollowsChanged = pk == pubKey event' }

                    GiftWrap -> do
                        kp <- getKeyPair
                        mSealedEvent <- unwrapGiftWrap event' kp
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
                        pure $ emptyUpdates { generalRelaysChanged = pk == pubKey event' }

                    PreferredDMRelays -> do
                        kp <- getKeyPair
                        let pk = keyPairToPubKeyXO kp
                        pure $ emptyUpdates { dmRelaysChanged = pk == pubKey event' }

                    _ -> do
                        logDebug $ "Ignoring event of kind: " <> pack (show (kind event'))
                        pure $ emptyUpdates

        when (myFollowsChanged updates || dmRelaysChanged updates || generalRelaysChanged updates) $ do
            -- notify the inbox model to update the subscriptions
            -- @TODO
            q <- gets @RelayPool updateQueue
            atomically $ writeTQueue q ()

        --logDebug $ "Event: " <> pack (show event')
        --logDebug $ "Notifying updates: " <> pack (show updates)
        when (updates /= emptyUpdates) $ do
            notify updates
