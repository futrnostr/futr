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
                                , readTQueue, readTVar, tryReadTQueue, writeTChan, writeTVar )
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Network.URI (URI(..), parseURI, uriAuthority, uriRegName, uriScheme)
import System.Random (randomIO)


import QtQuick
import KeyMgmt (AccountId(..), KeyMgmt, updateProfile)
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (validateEvent)
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
    NewSubscriptionId :: Subscription m SubscriptionId
    Subscribe :: RelayURI -> SubscriptionId -> Filter -> Subscription m (Maybe (TQueue SubscriptionEvent))
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
    NewSubscriptionId -> generateRandomSubscriptionId

    Subscribe r subId' f -> createSubscription r subId' f

    StopSubscription subId' -> do
        st <- get @RelayPool
        forM_ (Map.toList $ activeConnections st) $ \(r, rd) -> do
            case Map.lookup subId' (activeSubscriptions rd) of
                Just _ -> do
                    atomically $ writeTChan (requestChannel rd) (NT.Close subId')
                    modify @RelayPool $ \s -> s 
                        { activeConnections = Map.adjust
                            (\rd' -> rd' { activeSubscriptions = Map.delete subId' (activeSubscriptions rd') })
                            r
                            (activeConnections s)
                        }
                Nothing -> return ()

    HandleEvent r event' -> handleEvent' r event'

    StopAllSubscriptions relayUri -> do
        st <- get @RelayPool
        case Map.lookup relayUri (activeConnections st) of
            Just rd -> do
                forM_ (Map.keys $ activeSubscriptions rd) $ \subId -> do
                    atomically $ writeTChan (requestChannel rd) (NT.Close subId)
                modify @RelayPool $ \s -> s 
                    { activeConnections = Map.adjust
                        (\rd' -> rd' { activeSubscriptions = Map.empty })
                        relayUri
                        (activeConnections s)
                    }
            Nothing -> return ()


handleEvent' :: SubscriptionEff es => RelayURI -> Event -> Eff es UIUpdates
handleEvent' r event' = do
    --logDebug $ "Starting handleEvent' for event: " <> pack (show event')
    let ev = EventWithRelays event' (Set.singleton r)

    if not (validateEvent event')
        then do
            logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))
            pure emptyUpdates
        else do
            putEvent ev
            updates <- case kind event' of
                ShortTextNote -> 
                    pure $ emptyUpdates { postsChanged = True }

                Repost -> 
                    case ([t | t@(ETag _ _ _) <- tags event'], eitherDecode (fromStrict $ encodeUtf8 $ content event')) of
                        (ETag eid _ _:_, Right originalEvent) | validateEvent originalEvent -> 
                            pure $ emptyUpdates { postsChanged = True }
                        _ -> do
                            logWarning $ "Invalid repost or missing e-tag: " <> (byteStringToHex $ getEventId (eventId event'))
                            pure emptyUpdates

                EventDeletion -> 
                    pure $ emptyUpdates { postsChanged = True, privateMessagesChanged = True }

                Metadata -> do
                    case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                        Right profile -> do
                            st <- get @AppState
                            let isOwnProfile = maybe False (\kp -> pubKey event' == keyPairToPubKeyXO kp) (keyPair st)
                            when isOwnProfile $ do
                                let aid = AccountId $ pubKeyXOToBech32 (pubKey event')
                                updateProfile aid profile
                            pure $ emptyUpdates { profilesChanged = True }
                        Left err -> do
                            logWarning $ "Failed to decode metadata: " <> pack err
                            pure emptyUpdates

                FollowList -> 
                    pure $ emptyUpdates { followsChanged = True }

                GiftWrap -> do
                    pure $ emptyUpdates { privateMessagesChanged = True }

                RelayListMetadata -> do
                    let validRelayTags = [ r' | RelayTag r' <- tags event', isValidRelayURI (getUri r') ]
                    case validRelayTags of
                        [] -> do
                            logWarning $ "No valid relay URIs found in RelayListMetadata event from "
                                <> (pubKeyXOToBech32 $ pubKey event')
                            pure emptyUpdates
                        relays -> do
                            {- @todo: handle relay list update
                            handleRelayListUpdate (pubKey event') relays (createdAt event')
                                importGeneralRelays
                                generalRelays
                            -}
                            pure $ emptyUpdates { generalRelaysChanged = True }

                PreferredDMRelays -> do
                    let validRelayTags = [ r' | RelayTag r' <- tags event', isValidRelayURI (getUri r') ]
                    case validRelayTags of
                        [] -> do
                            logWarning $ "No valid relay URIs found in PreferredDMRelays event from "
                                <> (pubKeyXOToBech32 $ pubKey event')
                            pure emptyUpdates
                        relays -> do
                            {- @todo: handle relay list update
                            handleRelayListUpdate (pubKey event') relays (createdAt event')
                                importDMRelays
                                dmRelays
                            -}
                            pure $ emptyUpdates { dmRelaysChanged = True }

                _ -> do
                    logDebug $ "Ignoring event of kind: " <> pack (show (kind event'))
                    pure emptyUpdates
            --logDebug $ "Finished handleEvent' for event: " <> pack (show event')
            pure updates


-- | Create a subscription
createSubscription :: SubscriptionEff es
                   => RelayURI
                   -> SubscriptionId
                   -> Filter
                   -> Eff es (Maybe (TQueue SubscriptionEvent))
createSubscription r subId' f = do
    st <- get @RelayPool
    case Map.lookup r (activeConnections st) of
        Just rd -> do
            let channel = requestChannel rd
            atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' f)
            logDebug $ "Subscribed to " <> r <> " with subId " <> subId' <> " and filter " <> pack (show f)
            q <- newTQueueIO
            modify @RelayPool $ \st' ->
                st { activeConnections = Map.adjust
                        (\rd' -> rd' { activeSubscriptions = Map.insert subId' (SubscriptionDetails subId' f q 0 0) (activeSubscriptions rd') })
                        r
                        (activeConnections st')
                    }
            return $ pure q
        Nothing -> do
            logWarning $ "Cannot start subscription: no connection found for relay: " <> r
            return Nothing

{-
-- | Determine relay type and start appropriate subscriptions
handleRelaySubscription :: SubscriptionEff es => RelayURI -> Eff es ()
handleRelaySubscription r = do
    kp <- getKeyPair
    let pk = keyPairToPubKeyXO kp
    follows <- getFollows pk
    let followPks = map pubkey follows
    st' <- get @RelayPool

    -- Check if it's a DM relay
    let isDM = any (\(_, (relays, _)) ->
            any (\relay -> getUri relay == r) relays)
            (Map.toList $ dmRelays st')

    -- Check if it's an inbox-capable relay
    let isInbox = any (\(_, (relays, _)) ->
            any (\relay -> case relay of
                InboxRelay uri -> uri == r
                InboxOutboxRelay uri -> uri == r
                _ -> False) relays)
            (Map.toList $ generalRelays st')

    -- Start appropriate subscriptions based on relay type
    let f = if isDM
                then Just $ giftWrapFilter pk
                else if isInbox
                    then Just $ createInboxRelayFilter pk followPks
                    else Nothing

    --logInfo $ "Starting subscription for " <> r <> " with filter " <> pack (show fs)

    case f of
        Just f' -> do
            subId' <- generateRandomSubscriptionId
            mq <- createSubscription r subId' f'
            case mq of
                Just q -> do
                    shouldStop <- newTVarIO False

                    let loop = do
                            e <- atomically $ readTQueue q
                            es <- atomically $ flushTQueue q

                            updates <- fmap mconcat $ forM (e : es) $ \case
                                EventAppeared event' -> handleEvent' event' r
                                SubscriptionEose -> return emptyUpdates
                                SubscriptionClosed _ -> do
                                    atomically $ writeTVar shouldStop True
                                    return emptyUpdates

                            notify updates

                            shouldStopNow <- atomically $ readTVar shouldStop

                            if shouldStopNow
                                then return ()
                                else loop

                    loop
                Nothing -> logWarning $ "Failed to start subscription for " <> r
        Nothing -> return () -- Outbox only relay or unknown relay, no subscriptions needed


-- | Handle relay list updates with connection management
handleRelayListUpdate :: SubscriptionEff es
                     => PubKeyXO                                             -- ^ Public key of the event author
                     -> [Relay]                                             -- ^ New relay list
                     -> Int                                                 -- ^ Event timestamp
                     -> (PubKeyXO -> [Relay] -> Int -> Eff es ())          -- ^ Import function
                     -> (RelayPool -> Map.Map PubKeyXO ([Relay], Int)) -- ^ Relay map selector
                     -> Eff es ()
handleRelayListUpdate pk relays ts importFn getRelayMap = do
    st <- get @RelayPool
    let currentMap = getRelayMap st
    let (currentRelays, currentTs) = Map.findWithDefault ([], 0) pk currentMap

    when (ts > currentTs) $ do
        -- Log what we're doing
        logDebug $ "Updating relays for " <> pubKeyXOToBech32 pk 
                <> " from " <> pack (show currentRelays) 
                <> " to " <> pack (show relays)
                <> " (map: " <> pack (show $ Map.keys currentMap) <> ")"

        -- Disconnect from removed relays
        let removedRelays = filter (\r -> not $ any (\r' -> getUri r == getUri r') relays) currentRelays
        forM_ removedRelays $ \relay -> do
            logDebug $ "Disconnecting from removed relay: " <> pack (show (getUri relay))
            disconnectRelay (getUri relay)

        -- Import new configuration
        importFn pk relays ts

        -- Connect to new relays
        let newRelays = filter (\r -> not $ any (\r' -> getUri r == getUri r') currentRelays) relays
        forM_ newRelays $ \relay -> do
            logDebug $ "Connecting to new relay: " <> pack (show (getUri relay))
            void $ async $ connectRelay (getUri relay)


-- | Create DM relay subscription filter
createDMRelayFilter :: PubKeyXO -> [PubKeyXO] -> Filter
createDMRelayFilter xo followedPubKeys = giftWrapFilter xo


-- | Create inbox relay subscription filter
createInboxRelayFilter :: PubKeyXO -> [PubKeyXO] -> Filter
createInboxRelayFilter xo followedPubKeys =
    {-
    [ NT.followListFilter (xo : followedPubKeys)
    , NT.metadataFilter (xo : followedPubKeys)
    , NT.shortTextNoteFilter (xo : followedPubKeys)
    , NT.preferredDMRelaysFilter (xo : followedPubKeys)
    ]
    -}
    Filter
        { ids = Nothing
        , authors = Just (xo : followedPubKeys)
        , kinds = Just [FollowList, Metadata, ShortTextNote, EventDeletion, Repost, PreferredDMRelays]
        , since = Nothing
        , NT.until = Nothing
        , limit = Just 1000
        , fTags = Nothing
        }
-}
-- | Generate a random subscription ID
generateRandomSubscriptionId :: SubscriptionEff es => Eff es SubscriptionId
generateRandomSubscriptionId = do
    bytes <- liftIO $ replicateM 8 randomIO
    let byteString = BS.pack bytes
    return $ decodeUtf8 $ B16.encode byteString


-- | Subscribe to reposts for a specific event
subscribeToReposts :: SubscriptionEff es => EventId -> RelayURI -> Eff es (Maybe (TQueue SubscriptionEvent))
subscribeToReposts eid relayUri = do
    subId <- generateRandomSubscriptionId
    createSubscription relayUri subId (repostsFilter eid)


-- | Subscribe to comments for a specific event
subscribeToComments :: SubscriptionEff es => EventId -> RelayURI -> Eff es (Maybe (TQueue SubscriptionEvent))
subscribeToComments eid relayUri = do
    subId <- generateRandomSubscriptionId
    createSubscription relayUri subId (commentsFilter eid)


subscribeToFollowers :: SubscriptionEff es => PubKeyXO -> RelayURI -> Eff es (Maybe (TQueue SubscriptionEvent))
subscribeToFollowers xo relayUri = do
    subId <- generateRandomSubscriptionId
    createSubscription relayUri subId (followersFilter xo)


subscribeToFollowing :: SubscriptionEff es => PubKeyXO -> RelayURI -> Eff es (Maybe (TQueue SubscriptionEvent))
subscribeToFollowing xo relayUri = do
    subId <- generateRandomSubscriptionId
    createSubscription relayUri subId (followingFilter xo)


-- Helper function to count events from a subscription
countEvents :: SubscriptionEff es => TQueue SubscriptionEvent -> Eff es Int
countEvents queue = do
    event <- atomically $ tryReadTQueue queue
    case event of
        Just (EventAppeared _) -> (1 +) <$> countEvents queue
        Just SubscriptionEose -> return 0
        Nothing -> return 0


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
    , kinds = Just [RelayListMetadata, PreferredDMRelays, FollowList]
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
metadataFilter :: [PubKeyXO] -> Filter
metadataFilter authors = emptyFilter { authors = Just authors, kinds = Just [Metadata] }

-- | Creates a filter for short text notes.
shortTextNoteFilter :: [PubKeyXO] -> Filter
shortTextNoteFilter authors = emptyFilter { authors = Just authors, kinds = Just [ShortTextNote, EventDeletion, Repost] }

-- | Creates filter for gift wrapped messages.
giftWrapFilter :: PubKeyXO -> Maybe Int -> Filter
giftWrapFilter xo lastTimestamp = emptyFilter { kinds = Just [GiftWrap], fTags = Just $ Map.fromList [('p', [byteStringToHex $ exportPubKeyXO xo])], since = lastTimestamp }

-- | Creates a filter for preferred DM relays.
preferredDMRelaysFilter :: [PubKeyXO] -> Filter
preferredDMRelaysFilter authors = emptyFilter { authors = Just authors, kinds = Just [PreferredDMRelays] }

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
followersFilter pubkey = emptyFilter { kinds = Just [FollowList], fTags = Just $ Map.singleton 'p' [byteStringToHex $ exportPubKeyXO pubkey] }

-- | Filter for following a specific public key.
followingFilter :: PubKeyXO -> Filter
followingFilter pubkey = emptyFilter { authors = Just [pubkey], kinds = Just [FollowList] }

-- | Filter for mentions of a specific public key.
mentionsFilter :: PubKeyXO -> Filter
mentionsFilter pubkey = emptyFilter { kinds = Just [ShortTextNote, Repost, Comment, EventDeletion], fTags = Just $ Map.singleton 'p' [byteStringToHex $ exportPubKeyXO pubkey] }
