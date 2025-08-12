module Nostr.Subscription where

import Control.Monad (forM_, replicateM)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Map.Strict qualified as Map
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (forConcurrently)
import Effectful.Concurrent.STM (atomically, writeTChan)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, modify)
import qualified Nostr.RelayPool as RelayPool
import Prelude hiding (until)
import System.Random (randomIO)

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
import Nostr.RelayPool (RelayPoolState(..), SubscriptionState(..))

-- | Subscription effects
data Subscription :: Effect where
    Subscribe :: RelayURI -> Filter -> Subscription m SubscriptionId
    SubscribeTemporary :: RelayURI -> Filter -> Subscription m SubscriptionId
    StopSubscription :: SubscriptionId -> Subscription m ()
    StopAllSubscriptions :: RelayURI -> Subscription m ()


type instance DispatchOf Subscription = Dynamic


subscribe :: Subscription :> es => RelayURI -> Filter -> Eff es SubscriptionId
subscribe uri filter' = send $ Subscribe uri filter'

subscribeTemporary :: Subscription :> es => RelayURI -> Filter -> Eff es SubscriptionId
subscribeTemporary uri filter' = send $ SubscribeTemporary uri filter'

stopSubscription :: Subscription :> es => SubscriptionId -> Eff es ()
stopSubscription subId = send $ StopSubscription subId

stopAllSubscriptions :: Subscription :> es => RelayURI -> Eff es ()
stopAllSubscriptions uri = send $ StopAllSubscriptions uri


-- | SubscriptionEff
type SubscriptionEff es =
  ( State AppState :> es
  , State RelayPool.RelayPoolState :> es
  , RelayPool.RelayPool :> es
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
    Subscribe r f -> do
        registerNewSub False r f

    SubscribeTemporary r f -> do
        registerNewSub True r f

    StopSubscription subId' -> do
        st <- get @RelayPool.RelayPoolState

        case Map.lookup subId' (subscriptions st) of
            Just subDetails -> do
                let relayUri = relay subDetails
                case Map.lookup relayUri (activeConnections st) of
                    Just rd -> atomically $ writeTChan (RelayPool.requestChannel rd) (NT.Close subId')
                    Nothing -> pure ()

                RelayPool.deleteSubscriptionId subId'
            Nothing -> pure ()

        case Map.lookup subId' (pendingSubscriptions st) of
            Just _ -> do
                RelayPool.setPendingSubscriptions (Map.delete subId' (pendingSubscriptions st))
            Nothing -> pure ()

    StopAllSubscriptions relayUri -> do
        st <- get @RelayPool.RelayPoolState
        case Map.lookup relayUri (activeConnections st) of
            Just rd -> do
                relaySubIds <- RelayPool.listSubscriptionIdsForRelay relayUri

                forM_ relaySubIds $ \subId -> do
                    atomically $ writeTChan (RelayPool.requestChannel rd) (NT.Close subId)
                -- Remove all subscriptions for this relay from state immediately.
                -- Relying on relays to send CLOSED leads to leaks because Nostr servers
                -- are not required to acknowledge client CLOSE with a CLOSED message.
                RelayPool.deleteSubscriptionsBulk relaySubIds
            Nothing -> return ()

        let affectedPendingSubs = [ subDetails
                                 | (_, subDetails) <- Map.toList (pendingSubscriptions st)
                                 , relay subDetails == relayUri
                                 ]
        -- No queue to notify; simply remove them

        RelayPool.filterPendingSubscriptionsByRelay relayUri

-- | Helper to create and register a new subscription, toggling temporary flag.
registerNewSub :: SubscriptionEff es => Bool -> RelayURI -> Filter -> Eff es SubscriptionId
registerNewSub isTemp r f = do
    subId' <- generateRandomSubscriptionId
    let sub = SubscriptionState f r 0 (maxBound :: Int) False isTemp (Just f)
    registerSubscription r subId' sub f

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


-- | Creates a filter for fetching metadata.
metadataFilter :: [PubKeyXO] -> Filter
metadataFilter pks = emptyFilter
    { authors = Just pks
    , kinds = Just [Metadata]
    , limit = Just $ 50 * length pks
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
    -> RelayPool.SubscriptionState
    -> Filter
    -> Eff es SubscriptionId
registerSubscription r subId' sub f = do
    st <- get @RelayPool.RelayPoolState
    case Map.lookup r (activeConnections st) of
        Just rd -> do
            let channel = RelayPool.requestChannel rd
            RelayPool.insertSubscription subId' sub
            atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' f)
            return subId'
        Nothing -> do
            -- Optimization: if relay is disconnected but has an active channel, we still register pending
            RelayPool.insertPendingSubscription subId' sub
            return subId'


-- | Subscribe the same temporary filter to multiple relays concurrently.
--   Returns the list of created SubscriptionIds (one per relay).
subscribeMultiTemporary
  :: SubscriptionEff es
  => [RelayURI]
  -> Filter
  -> Eff es [SubscriptionId]
subscribeMultiTemporary relayUris f = do
  forConcurrently relayUris $ \uri -> registerNewSub True uri f


-- | Await until all given subscriptions have reached EOSE (or were removed) or until timeout (seconds) expires.
--   Returns True if all finished before timeout, False otherwise. On timeout, sends CLOSE to any remaining subs.
awaitAllEose
  :: SubscriptionEff es
  => [SubscriptionId]
  -> Int          -- ^ timeout in seconds
  -> Eff es Bool
awaitAllEose subIds timeoutSeconds = do
  start <- getCurrentTime
  let loop = do
        now <- getCurrentTime
        st <- get @RelayPool.RelayPoolState
        let done sid = case Map.lookup sid (subscriptions st) of
                         Nothing -> True                   -- already closed/removed → treat as done
                         Just sd -> eoseReceived sd == True
        if all done subIds
          then pure True
          else if now - start >= timeoutSeconds
                 then pure False
                 else do
                   threadDelay 100000  -- 100ms
                   loop
  result <- loop
  -- On timeout, proactively close remaining subs
  if result
    then pure True
    else do
      st <- get @RelayPool.RelayPoolState
      let stillOpen = [ sid | sid <- subIds, Map.member sid (subscriptions st) ]
      mapM_ closeSubInline stillOpen
      pure False
  where
    closeSubInline :: SubscriptionEff es => SubscriptionId -> Eff es ()
    closeSubInline subId' = do
      st <- get @RelayPool.RelayPoolState
      case Map.lookup subId' (subscriptions st) of
        Just subDetails -> do
          let relayUri = relay subDetails
          case Map.lookup relayUri (activeConnections st) of
            Just rd -> atomically $ writeTChan (RelayPool.requestChannel rd) (NT.Close subId')
            Nothing -> pure ()
          RelayPool.deleteSubscriptionId subId'
        Nothing -> pure ()
      case Map.lookup subId' (pendingSubscriptions st) of
        Just _ -> RelayPool.setPendingSubscriptions (Map.delete subId' (pendingSubscriptions st))
        Nothing -> pure ()
