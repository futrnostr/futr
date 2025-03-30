module Nostr.EventHandler where

import Control.Monad (when)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (fromStrict)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (atomically, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, gets)
import Effectful.TH
import Prelude hiding (until)

import KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (Event(..), EventId(..), Kind(..), validateEvent)
import Nostr.Keys (byteStringToHex, keyPairToPubKeyXO)
import Nostr.Relay (RelayURI)
import Nostr.Util
import QtQuick
import Store.Lmdb
import Types

-- | Event handler effects
data EventHandler :: Effect where
    HandleEvent :: Maybe RelayURI -> Event -> EventHandler m ()

type instance DispatchOf EventHandler = Dynamic

makeEffect ''EventHandler

-- | EventHandlerEff
type EventHandlerEff es =
  ( LmdbStore :> es
  , KeyMgmt :> es
  , State RelayPool :> es
  , Util :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
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
                let set = case mr of
                        Just r -> Set.singleton r
                        Nothing -> Set.empty

                let ev = EventWithRelays event' set
                wasUpdated <- putEvent ev

                case kind event' of
                    ShortTextNote -> do
                        pure $ emptyUpdates { postsChanged = wasUpdated }

                    Repost -> do
                        pure $ emptyUpdates { postsChanged = wasUpdated }

                    EventDeletion ->
                        pure $ emptyUpdates { postsChanged = wasUpdated, privateMessagesChanged = wasUpdated }

                    Metadata -> do
                        --logDebug $ "Received metadata event: " <> pack (show $ eventId event')
                        case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                            Right profile -> do
                                kp <- getKeyPair
                                let isOwnProfile = pubKey event' == keyPairToPubKeyXO kp
                                when isOwnProfile $ do
                                    let aid = AccountId $ pubKeyXOToBech32 (pubKey event')
                                    updateProfile aid profile
                                pure $ emptyUpdates { profilesChanged = wasUpdated, myFollowsChanged = wasUpdated }
                            Left err -> do
                                logWarning $ "Failed to decode metadata: " <> pack err
                                pure emptyUpdates

                    FollowList -> do
                        kp <- getKeyPair
                        let pk = keyPairToPubKeyXO kp
                        pure $ emptyUpdates { followsChanged = wasUpdated, myFollowsChanged = wasUpdated && pk == pubKey event' }

                    GiftWrap -> do
                        pure $ emptyUpdates { privateMessagesChanged = wasUpdated }

                    RelayListMetadata -> do
                        pure $ emptyUpdates { generalRelaysChanged = wasUpdated }

                    PreferredDMRelays -> do
                        pure $ emptyUpdates { dmRelaysChanged = wasUpdated }

                    _ -> do
                        logDebug $ "Ignoring event of kind: " <> pack (show (kind event'))
                        pure emptyUpdates

        when (myFollowsChanged updates || dmRelaysChanged updates || generalRelaysChanged updates) $ do
            -- notify the inbox model to update the subscriptions
            q <- gets @RelayPool updateQueue
            atomically $ writeTQueue q ()

        --logDebug $ "Event: " <> pack (show event')
        --logDebug $ "Notifying updates: " <> pack (show updates)
        notify updates
