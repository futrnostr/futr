module Nostr.Subscription where

import Control.Monad (forM_, replicateM)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Map.Strict qualified as Map
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (atomically, newTQueueIO, writeTChan, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Prelude hiding (until)
import System.Random (randomIO)

-- debug imports
--import Crypto.Hash.SHA256 qualified as SHA256
--import Data.Aeson (encode)

import QtQuick
import KeyMgmt (KeyMgmt)
import Logging
import Nostr.Event (EventId(..), Kind(..))
import Nostr.Keys (PubKeyXO, byteStringToHex, exportPubKeyXO)
import Nostr.Relay (RelayURI)
import Nostr.RelayConnection
import Nostr.Types (Filter(..), SubscriptionId, emptyFilter)
import Nostr.Types qualified as NT
import Nostr.Util
import RelayMgmt
import Store.Lmdb
import Types


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


-- | Subscription effects
data Subscription :: Effect where
    Subscribe :: RelayURI -> Filter -> Subscription m SubscriptionId
    StopSubscription :: SubscriptionId -> Subscription m ()
    --HandleEvent :: RelayURI -> Event -> Subscription m UIUpdates
    StopAllSubscriptions :: RelayURI -> Subscription m ()


type instance DispatchOf Subscription = Dynamic

makeEffect ''Subscription

-- | Handler for subscription effects.
runSubscription
  :: SubscriptionEff es
  => Eff (Subscription : es) a
  -> Eff es a
runSubscription = interpret $ \_ -> \case
    Subscribe r f -> do
        q <- newTQueueIO
        subId' <- generateRandomSubscriptionId
        let sub = newSubscriptionState f q r
        registerSubscription r subId' sub f

    StopSubscription subId' -> do
        st <- get @RelayPool

        case Map.lookup subId' (subscriptions st) of
            Just subDetails -> do
                let relayUri = relay subDetails
                case Map.lookup relayUri (activeConnections st) of
                    Just rd -> atomically $ writeTChan (requestChannel rd) (NT.Close subId')
                    Nothing -> pure ()

                atomically $ writeTQueue (responseQueue subDetails) (relayUri, SubscriptionClosed "Subscription stopped")
                modify @RelayPool $ \s -> s { subscriptions = Map.delete subId' (subscriptions s) }
            Nothing -> pure ()

        case Map.lookup subId' (pendingSubscriptions st) of
            Just subDetails -> do
                atomically $ writeTQueue (responseQueue subDetails) (relay subDetails, SubscriptionClosed "Subscription stopped")
                modify @RelayPool $ \s -> s { pendingSubscriptions = Map.delete subId' (pendingSubscriptions s) }
            Nothing -> pure ()

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
            { pendingSubscriptions = Map.filterWithKey (\_ v -> relay v /= relayUri) (pendingSubscriptions s) }

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
profilesFilter :: [PubKeyXO] -> Filter
profilesFilter pks = emptyFilter
    { authors = Just pks
    , kinds = Just [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
    , limit = Just $ 500 * length pks
    }


-- | Creates a filter for fetching a user's public posts and interactions.
--
-- This filter targets three event kinds:
-- * ShortTextNote - User's regular posts
-- * Repost        - Content the user has reposted
-- * EventDeletion - User's delete requests
userPostsFilter :: [PubKeyXO] -> Maybe Int -> Maybe Int -> Filter
userPostsFilter pks s ml = emptyFilter
    { authors = Just pks
    , kinds = Just [ShortTextNote, Repost, EventDeletion]
    , since = s
    , limit = case ml of
        Just l -> Just l
        Nothing -> Just $ 500 * length pks
    }


-- | Creates a filter for metadata.
metadataFilter :: [PubKeyXO] -> Filter
metadataFilter pks = emptyFilter { authors = Just pks, kinds = Just [Metadata] }

-- | Creates a filter for short text notes.
shortTextNoteFilter :: [PubKeyXO] -> Filter
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
commentsFilter eid = emptyFilter
    { kinds = Just [ShortTextNote]
    , fTags = Just $ Map.singleton 'e' [decodeUtf8 $ B16.encode $ getEventId eid]
    , limit = Just $ 2000
    }

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
    , since = ts
    , limit = Just $ 2000
    }

-- | Register a new subscription with the relay pool and optionally start it if the relay is connected
registerSubscription
    :: SubscriptionEff es
    => RelayURI
    -> SubscriptionId
    -> SubscriptionState
    -> Filter
    -> Eff es SubscriptionId
registerSubscription r subId' sub f = do
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
