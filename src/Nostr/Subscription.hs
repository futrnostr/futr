module Nostr.Subscription where

import Control.Monad (forM_, replicateM, when)
import Data.Aeson (eitherDecode)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy (fromStrict)
import Data.Map.Strict qualified as Map
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TQueue, atomically, newTQueueIO, writeTChan)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Network.URI (URI(..), parseURI, uriAuthority, uriRegName, uriScheme)
import System.Random (randomIO)

import EffectfulQML
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (validateEvent)
import Nostr.GiftWrap (GiftWrap, handleGiftWrapEvent)
import Nostr.Keys (PubKeyXO, byteStringToHex, keyPairToPubKeyXO)
import Nostr.Types ( Event(..), EventId(..), Filter, Kind(..), Relay(..)
                   , RelayURI, SubscriptionId, Tag(..), getUri )
import Nostr.Types qualified as NT
import Presentation.KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import RelayMgmt
import Types


-- | Subscription effects
data Subscription :: Effect where
    NewSubscriptionId :: Subscription m SubscriptionId
    Subscribe :: RelayURI -> SubscriptionId -> [Filter] -> Subscription m (Maybe (TQueue SubscriptionEvent))
    StopSubscription :: SubscriptionId -> Subscription m ()
    HandleEvent :: RelayURI -> SubscriptionId -> [Filter] -> Event -> Subscription m UIUpdates

type instance DispatchOf Subscription = Dynamic

makeEffect ''Subscription


-- | SubscriptionEff
type SubscriptionEff es =
  ( State AppState :> es
  , State RelayPoolState :> es
  , GiftWrap :> es
  , KeyMgmt :> es
  , RelayMgmt :> es
  , Logging :> es
  , Concurrent :> es
  , EffectfulQML :> es
  , IOE :> es
  )

-- | Handler for subscription effects.
runSubscription
  :: SubscriptionEff es
  => Eff (Subscription : es) a
  -> Eff es a
runSubscription = interpret $ \_ -> \case
    NewSubscriptionId -> do
        bytes <- liftIO $ replicateM 8 randomIO
        let byteString = BS.pack bytes
        return $ decodeUtf8 $ B16.encode byteString

    Subscribe r subId' fs -> do
        st <- get @RelayPoolState
        case Map.lookup r (activeConnections st) of
            Nothing -> do
                logWarning $ "Cannot start subscription: no connection found for relay: " <> r
                return Nothing
            Just rd -> do
                let channel = requestChannel rd
                atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' fs)
                q <- newTQueueIO
                modify @RelayPoolState $ \st' ->
                    st { activeConnections = Map.adjust
                            (\rd' -> rd' { activeSubscriptions = Map.insert subId' (SubscriptionDetails subId' fs q 0 0) (activeSubscriptions rd') })
                            r
                            (activeConnections st')
                       }
                return $ pure q

    StopSubscription subId' -> do
        st <- get @RelayPoolState
        forM_ (Map.toList $ activeConnections st) $ \(r, rd) -> do
            case Map.lookup subId' (activeSubscriptions rd) of
                Just _ -> do
                    atomically $ writeTChan (requestChannel rd) (NT.Close subId')
                    modify @RelayPoolState $ \s -> s 
                        { activeConnections = Map.adjust
                            (\rd' -> rd' { activeSubscriptions = Map.delete subId' (activeSubscriptions rd') })
                            r
                            (activeConnections s)
                        }
                Nothing -> return ()

    HandleEvent _ _ _ event' -> do
        -- @todo validate event against filters ??
        if not (validateEvent event')
        then do
            logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))
            pure emptyUpdates
        else do
            logDebug $ "Handling event of kind " <> pack (show $ kind event') <> " with id " <> (byteStringToHex $ getEventId (eventId event'))
            case kind event' of
                Metadata -> do
                    case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                        Right profile -> do
                            st <- get @AppState
                            let isOwnProfile = maybe False (\kp -> pubKey event' == keyPairToPubKeyXO kp) (keyPair st)

                            modify $ \s -> s { profiles = Map.insertWith (\new old -> if snd new > snd old then new else old)
                                                (pubKey event')
                                                (profile, createdAt event')
                                                (profiles s)
                                            }

                            when isOwnProfile $ do
                                let aid = AccountId $ pubKeyXOToBech32 (pubKey event')
                                updateProfile aid profile
                            
                            pure $ emptyUpdates { profilesChanged = True }
                        Left err -> do
                            logWarning $ "Failed to decode metadata: " <> pack err
                            pure emptyUpdates

                FollowList -> do
                    let followList' = [Follow pk (fmap InboxRelay relay') petName' | PTag pk relay' petName' <- tags event']
                    modify $ \st -> st { follows = Map.insert (pubKey event') followList' (follows st) }
                    pure $ emptyUpdates { followsChanged = True }

                GiftWrap -> do
                    handleGiftWrapEvent event'
                    pure $ emptyUpdates { chatsChanged = True }

                RelayListMetadata -> do
                    logDebug $ pack $ show event'
                    let validRelayTags = [ r' | RelayTag r' <- tags event', isValidRelayURI (getUri r') ]
                        ts = createdAt event'
                        pk = pubKey event'
                    case validRelayTags of
                        [] -> do
                            logWarning $ "No valid relay URIs found in RelayListMetadata event from " 
                                <> (pubKeyXOToBech32 pk)
                            pure emptyUpdates
                        relays -> do
                            importGeneralRelays pk relays ts
                            -- @todo auto connect to new relays, disconnect from old ones
                            -- IF the event is from our pubkey
                            logDebug $ "Updated relay list for " <> (pubKeyXOToBech32 pk) 
                                <> " with " <> pack (show $ length relays) <> " relays"
                            pure $ emptyUpdates { generalRelaysChanged = True }

                PreferredDMRelays -> do
                    logDebug $ "Handling PreferredDMRelays event: " <> pack (show event')
                    let validRelayTags = [ r' | RelayTag r' <- tags event', isValidRelayURI (getUri r') ]
                    case validRelayTags of
                        [] -> do
                            logWarning $ "No valid relay URIs found in PreferredDMRelays event from " 
                                <> (pubKeyXOToBech32 $ pubKey event')
                            pure emptyUpdates
                        preferredRelays -> do
                            importDMRelays (pubKey event') preferredRelays (createdAt event')
                            -- @todo auto connect to new relays, disconnect from old ones
                            -- IF the event is from our pubkey
                            pure $ emptyUpdates { dmRelaysChanged = True }

                _ -> do
                    logDebug $ "Ignoring event of kind: " <> pack (show (kind event'))
                    pure emptyUpdates
        where
            isValidRelayURI :: RelayURI -> Bool
            isValidRelayURI uriText =
                case parseURI (unpack uriText) of
                    Just uri ->
                        let scheme = uriScheme uri
                            authority = uriAuthority uri
                        in (scheme == "ws:" || scheme == "wss:") &&
                            maybe False (not . null . uriRegName) authority
                    Nothing -> False


-- Keep the most recent relay list and timestamp
keepMostRecent :: ([Relay], Int) -> ([Relay], Int) -> ([Relay], Int)
keepMostRecent (newRelays, newTime) (oldRelays, oldTime)
    | newTime > oldTime = (newRelays, newTime)
    | otherwise = (oldRelays, oldTime)


updateGeneralRelays :: SubscriptionEff es => PubKeyXO -> [Relay] -> Int -> Eff es ()
updateGeneralRelays pk relays ts = do
    modify @RelayPoolState $ \st -> st 
        { generalRelays = Map.insertWith keepMostRecent
            pk
            (relays, ts)
            (generalRelays st)
        }
    notifyRelayStatus


updateDMRelays :: SubscriptionEff es => PubKeyXO -> [Relay] -> Int -> Eff es ()
updateDMRelays pk relays ts = do
    modify @RelayPoolState $ \st -> st 
        { dmRelays = Map.insertWith keepMostRecent
            pk
            (relays, ts)
            (dmRelays st)
        }
    notifyRelayStatus
