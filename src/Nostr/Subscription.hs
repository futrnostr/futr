module Nostr.Subscription where

import Control.Monad (forM, forM_, replicateM, unless, void, when)
import Data.Aeson (eitherDecode)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy (fromStrict)
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM ( TQueue, atomically, flushTQueue, newTQueueIO, newTVarIO
                                , readTQueue, readTVar, tryReadTQueue, writeTChan, writeTQueue, writeTVar )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Network.URI (URI(..), parseURI, uriAuthority, uriRegName, uriScheme)
import System.Random (randomIO)

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (encode)

import QtQuick
import KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (validateEvent, validateEventId, verifySignature)
import Nostr.Keys (PubKeyXO, byteStringToHex, exportPubKeyXO, keyPairToPubKeyXO)
import Nostr.RelayConnection
import Nostr.Types ( Event(..), EventId(..), Filter(..), Kind(..), Relay(..)
                   , RelayURI, SubscriptionId, Tag(..), emptyFilter, getUri, isValidRelayURI )
import Nostr.Types qualified as NT
import Nostr.Util
import RelayMgmt
import Store.Lmdb
import Types


-- | Subscription effects
data Subscription :: Effect where
    Subscribe
        :: RelayURI
        -> Filter
        -> TQueue (RelayURI, SubscriptionEvent)
        -> Subscription m SubscriptionId
    StopSubscription :: SubscriptionId -> Subscription m ()
    HandleEvent :: RelayURI -> Event -> Subscription m UIUpdates
    StopAllSubscriptions :: RelayURI -> Subscription m ()

type instance DispatchOf Subscription = Dynamic

makeEffect ''Subscription


-- | SubscriptionEff
type SubscriptionEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
  , RelayConnection :> es
  , KeyMgmt :> es
  , RelayMgmt :> es
  , Util :> es
  , Logging :> es
  , Concurrent :> es
  , QtQuick :> es
  , IOE :> es
  )

-- | Handler for subscription effects.
runSubscription
  :: SubscriptionEff es
  => Eff (Subscription : es) a
  -> Eff es a
runSubscription = interpret $ \_ -> \case
    Subscribe r f queue -> do
        logDebug $ "Subscribing to relay: " <> r <> " with filter: " <> pack (show f)
        subId' <- generateRandomSubscriptionId
        let sub = SubscriptionDetails subId' f queue 0 0 r
        st <- get @RelayPool
        case Map.lookup r (activeConnections st) of
            Just rd -> do
                let channel = requestChannel rd
                modify @RelayPool $ \st' ->
                    st' { subscriptions = Map.insert subId' sub (subscriptions st') }
                atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' f)
                return subId'
            Nothing -> do
                modify @RelayPool $ \st' ->
                    st' { pendingSubscriptions = Map.insert subId' sub (pendingSubscriptions st') }
                return subId'

    StopSubscription subId' -> do
        st <- get @RelayPool

        case Map.lookup subId' (subscriptions st) of
            Just subDetails -> do
                case Map.lookup (relay subDetails) (activeConnections st) of
                    Just rd -> atomically $ writeTChan (requestChannel rd) (NT.Close subId')
                    Nothing -> pure ()
            Nothing -> pure ()

        case Map.lookup subId' (pendingSubscriptions st) of
            Just subDetails ->
                atomically $ writeTQueue (responseQueue subDetails) (relay subDetails, SubscriptionClosed "Subscription stopped")
            Nothing -> pure ()

        modify @RelayPool $ \s -> s
            { subscriptions = Map.delete subId' (subscriptions s)
            , pendingSubscriptions = Map.delete subId' (pendingSubscriptions s)
            }

    HandleEvent r event' -> do
        --logDebug $ "Starting handleEvent' for event: " <> pack (show event')
        let ev = EventWithRelays event' (Set.singleton r)

        if not (validateEvent event')
            then do
                logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))
                {-
                logWarning $ "EventID: " <> if validateEventId event' then "valid" else "invalid"
                logWarning $ "Signature: " <> if verifySignature event' then "valid" else "invalid"


                let unsignedEvent = NT.UnsignedEvent (pubKey event') (createdAt event') (kind event') (tags event') (content event')
                    serializedEvent = BS.toStrict $ encode unsignedEvent
                    computedId = SHA256.hash serializedEvent
                    eventId' = getEventId $ eventId event'

                logWarning $ "Raw event: " <> pack (show serializedEvent)
                logWarning $ "Event: " <> pack (show event')
                -}
                pure emptyUpdates
            else do
                wasUpdated <- putEvent ev
                updates <- case kind event' of
                    ShortTextNote -> do
                        pure $ emptyUpdates { postsChanged = wasUpdated }

                    Repost -> do
                        pure $ emptyUpdates { postsChanged = wasUpdated }

                    EventDeletion ->
                        pure $ emptyUpdates { postsChanged = wasUpdated, privateMessagesChanged = wasUpdated }

                    Metadata -> do
                        case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                            Right profile -> do
                                st <- get @AppState
                                let isOwnProfile = maybe False (\kp -> pubKey event' == keyPairToPubKeyXO kp) (keyPair st)
                                when isOwnProfile $ do
                                    let aid = AccountId $ pubKeyXOToBech32 (pubKey event')
                                    updateProfile aid profile
                                pure $ emptyUpdates { profilesChanged = wasUpdated }
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
                --logDebug $ "Finished handleEvent' for event: " <> pack (show event')
                pure updates

    StopAllSubscriptions relayUri -> do
        st <- get @RelayPool
        case Map.lookup relayUri (activeConnections st) of
            Just rd -> do
                let relaySubIds = [ subId
                                | (subId, subDetails) <- Map.toList (subscriptions st)
                                , relay subDetails == relayUri
                                ]

                forM_ relaySubIds $ \subId -> do
                    atomically $ writeTChan (requestChannel rd) (NT.Close subId)
            Nothing -> return ()

        let affectedPendingSubs = [ subDetails
                                 | (_, subDetails) <- Map.toList (pendingSubscriptions st)
                                 , relay subDetails == relayUri
                                 ]

        forM_ affectedPendingSubs $ \subDetails ->
            atomically $ writeTQueue (responseQueue subDetails) (relay subDetails, SubscriptionClosed "Subscription stopped")

        modify @RelayPool $ \s -> s
            { subscriptions = Map.filterWithKey (\k v -> relay v /= relayUri) (subscriptions s)
            , pendingSubscriptions = Map.filterWithKey (\k v -> relay v /= relayUri) (pendingSubscriptions s)
            }


-- | Generate a random subscription ID
generateRandomSubscriptionId :: SubscriptionEff es => Eff es SubscriptionId
generateRandomSubscriptionId = do
    bytes <- liftIO $ replicateM 8 randomIO
    let byteString = BS.pack bytes
    return $ decodeUtf8 $ B16.encode byteString


-- Helper functions to create specific filters


-- | Creates a filter for fetching profile-related metadata events.
--
-- This filter targets three key event kinds:
-- * RelayListMetadata (Kind 10002) - User's preferred general-purpose relays
-- * PreferredDMRelays (Kind 10003) - User's preferred DM relays
-- * FollowList        (Kind 3)     - User's list of followed pubkeys
profilesFilter :: [PubKeyXO] -> Maybe Int -> Filter
profilesFilter authors lastTimestamp = emptyFilter 
    { authors = Just authors
    , kinds = Just [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
    , since = lastTimestamp
    }


-- | Creates a filter for fetching a user's public posts and interactions.
--
-- This filter targets three event kinds:
-- * ShortTextNote - User's regular posts
-- * Repost        - Content the user has reposted
-- * Comment       - User's replies to other posts
userPostsFilter :: [PubKeyXO] -> Maybe Int -> Filter
userPostsFilter authors lastTimestamp = emptyFilter 
    { authors = Just authors
    , kinds = Just [ShortTextNote, Repost, EventDeletion]
    , since = lastTimestamp
    }


-- | Creates a filter for metadata.
metadataFilter pks = emptyFilter { authors = Just pks, kinds = Just [Metadata] }

-- | Creates a filter for short text notes.
shortTextNoteFilter pks = emptyFilter { authors = Just pks, kinds = Just [ShortTextNote, EventDeletion, Repost] }

-- | Creates filter for gift wrapped messages.
giftWrapFilter :: PubKeyXO -> Maybe Int -> Filter
giftWrapFilter xo lastTimestamp = emptyFilter { kinds = Just [GiftWrap], fTags = Just $ Map.fromList [('p', [byteStringToHex $ exportPubKeyXO xo])], since = lastTimestamp }

-- | Creates a filter for preferred DM relays.
preferredDMRelaysFilter :: [PubKeyXO] -> Filter
preferredDMRelaysFilter pks = emptyFilter { authors = Just pks, kinds = Just [PreferredDMRelays] }

relayListMetadataFilter :: [PubKeyXO] -> Filter
relayListMetadataFilter pks = emptyFilter { authors = Just pks, kinds = Just [RelayListMetadata] }

-- | Creates a filter for a specific event by its ID.
eventFilter :: EventId -> Filter
eventFilter eid = emptyFilter { ids = Just [eid] }

-- | Filter for reposts of a specific event.
repostsFilter :: EventId -> Filter
repostsFilter eid = emptyFilter { kinds = Just [Repost], fTags = Just $ Map.singleton 'e' [decodeUtf8 $ B16.encode $ getEventId eid] }

-- | Filter for comments on a specific event.
commentsFilter :: EventId -> Filter
commentsFilter eid = emptyFilter { kinds = Just [ShortTextNote], fTags = Just $ Map.singleton 'e' [decodeUtf8 $ B16.encode $ getEventId eid] }

-- | Filter for followers of a specific public key.
followersFilter :: PubKeyXO -> Filter
followersFilter pk = emptyFilter { kinds = Just [FollowList], fTags = Just $ Map.singleton 'p' [byteStringToHex $ exportPubKeyXO pk] }

-- | Filter for following a specific public key.
followingFilter :: PubKeyXO -> Filter
followingFilter pk = emptyFilter { authors = Just [pk], kinds = Just [FollowList] }

-- | Filter for mentions of a specific public key.
mentionsFilter :: PubKeyXO -> Maybe Int ->Filter
mentionsFilter pk ts = emptyFilter
    { kinds = Just [ShortTextNote, Repost, Comment, EventDeletion]
    , fTags = Just $ Map.singleton 'p' [byteStringToHex $ exportPubKeyXO pk]
    , since = ts }
