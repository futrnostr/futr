module Nostr.Subscription where

import Control.Monad (forM, forM_, replicateM, unless, when)
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
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO, newTVarIO, readTQueue, readTVar, writeTChan, writeTVar)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Network.URI (URI(..), parseURI, uriAuthority, uriRegName, uriScheme)
import System.Random (randomIO)


import QtQuick
import KeyMgmt (AccountId(..), KeyMgmt, updateProfile, updateRelays)
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (validateEvent)
import Nostr.GiftWrap (GiftWrap, handleGiftWrapEvent)
import Nostr.Keys (PubKeyXO, byteStringToHex, keyPairToPubKeyXO)
import Nostr.RelayConnection
import Nostr.Types ( Event(..), EventId(..), Filter(..), Kind(..), Relay(..)
                   , RelayURI, SubscriptionId, Tag(..), getUri )
import Nostr.Types qualified as NT
import Nostr.Util
import RelayMgmt
import Store.Lmdb
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
  , LmdbStore :> es
  , GiftWrap :> es
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

    Subscribe r subId' fs -> createSubscription r subId' fs

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

    HandleEvent r _ _ event' -> handleEvent' event' r


handleEvent' :: SubscriptionEff es => Event -> RelayURI -> Eff es UIUpdates
handleEvent' event' r = do
    logDebug $ "Starting handleEvent' for event: " <> pack (show event')
    let ev = EventWithRelays event' (Set.singleton r)

    -- Validate event
    if not (validateEvent event')
    then do
        logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))
        pure emptyUpdates
    else case kind event' of
        ShortTextNote -> do
            addPostTimelineEntry ev
            pure $ emptyUpdates { postsChanged = True }

        Repost -> do
            case ([t | t@(ETag _ _ _) <- tags event'], eitherDecode (fromStrict $ encodeUtf8 $ content event')) of
                (ETag eid _ _:_, Right originalEvent) | validateEvent originalEvent -> do
                    addPostTimelineEntry ev
                    pure $ emptyUpdates { postsChanged = True }
                _ -> do
                    logWarning $ "Invalid repost or missing e-tag: " <> (byteStringToHex $ getEventId (eventId event'))
                    pure emptyUpdates

        EventDeletion -> do
            let eventIdsToDelete = [eid | ETag eid _ _ <- tags event']
            forM_ eventIdsToDelete $ \eid -> deleteTimelineEntry eid

            pure $ emptyUpdates
                { postsChanged = True
                , privateMessagesChanged = True
                }

        Metadata -> do
            case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                Right profile -> do
                    putProfile ev (profile, (createdAt event'))

                    st <- get @AppState
                    let isOwnProfile = maybe False (\kp -> pubKey event' == keyPairToPubKeyXO kp) (keyPair st)
                    when isOwnProfile $ do
                        let aid = AccountId $ pubKeyXOToBech32 (pubKey event')
                        updateProfile aid profile

                    pure $ emptyUpdates { profilesChanged = True }
                Left err -> do
                    logWarning $ "Failed to decode metadata: " <> pack err
                    pure emptyUpdates

        FollowList -> do
            let followList' = [Follow pk (fmap InboxRelay relay') petName' | PTag pk relay' petName' <- tags event']
            putFollows ev followList'
            pure $ emptyUpdates { followsChanged = True }

        GiftWrap -> do
            handleGiftWrapEvent event'
            pure $ emptyUpdates { privateMessagesChanged = True }

        RelayListMetadata -> do
            putEvent ev
            let validRelayTags = [ r' | RelayTag r' <- tags event', isValidRelayURI (getUri r') ]
                ts = createdAt event'
                pk = pubKey event'
            case validRelayTags of
                [] -> do
                    logWarning $ "No valid relay URIs found in RelayListMetadata event from "
                        <> (pubKeyXOToBech32 pk)
                    pure emptyUpdates
                relays -> do
                    handleRelayListUpdate pk relays ts
                        importGeneralRelays
                        generalRelays
                    pure $ emptyUpdates { generalRelaysChanged = True }

        PreferredDMRelays -> do
            putEvent ev
            let validRelayTags = [ r' | RelayTag r' <- tags event', isValidRelayURI (getUri r') ]
            case validRelayTags of
                [] -> do
                    logWarning $ "No valid relay URIs found in PreferredDMRelays event from "
                        <> (pubKeyXOToBech32 $ pubKey event')
                    pure emptyUpdates
                relays -> do
                    handleRelayListUpdate (pubKey event') relays (createdAt event')
                        importDMRelays
                        dmRelays
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


-- | Create a subscription
createSubscription :: SubscriptionEff es
                   => RelayURI
                   -> SubscriptionId
                   -> [Filter]
                   -> Eff es (Maybe (TQueue SubscriptionEvent))
createSubscription r subId' fs = do
    st <- get @RelayPoolState
    case Map.lookup r (activeConnections st) of
        Just rd -> do
            let channel = requestChannel rd
            atomically $ writeTChan channel (NT.Subscribe $ NT.Subscription subId' fs)
            logDebug $ "Subscribed to " <> r <> " with subId " <> subId' <> " and filters " <> pack (show fs)
            q <- newTQueueIO
            modify @RelayPoolState $ \st' ->
                st { activeConnections = Map.adjust
                        (\rd' -> rd' { activeSubscriptions = Map.insert subId' (SubscriptionDetails subId' fs q 0 0) (activeSubscriptions rd') })
                        r
                        (activeConnections st')
                    }
            return $ pure q
        Nothing -> do
            logWarning $ "Cannot start subscription: no connection found for relay: " <> r
            return Nothing


-- | Determine relay type and start appropriate subscriptions
handleRelaySubscription :: SubscriptionEff es => RelayURI -> Eff es ()
handleRelaySubscription r = do
    kp <- getKeyPair
    let pk = keyPairToPubKeyXO kp
    follows <- getFollows pk
    let followPks = map pubkey follows
    st' <- get @RelayPoolState

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
    let fs = if isDM then Just $ createDMRelayFilters pk followPks
            else if isInbox then Just $ createInboxRelayFilters pk followPks
            else Nothing

    --logInfo $ "Starting subscription for " <> r <> " with filters " <> pack (show fs)

    case fs of
        Just fs' -> do
            subId' <- generateRandomSubscriptionId
            mq <- createSubscription r subId' fs'
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
                      -> [Relay]                                              -- ^ New relay list
                      -> Int                                                  -- ^ Event timestamp
                      -> (PubKeyXO -> [Relay] -> Int -> Eff es ())            -- ^ Import function
                      -> (RelayPoolState -> Map.Map PubKeyXO ([Relay], Int))  -- ^ Relay map selector
                      -> Eff es ()
handleRelayListUpdate pk relays ts importFn getRelayMap = do
    st <- get @RelayPoolState
    case Map.lookup pk (getRelayMap st) of
        Just (existingRelays, oldTs) | oldTs < ts -> do
            -- Disconnect from relays that aren't in the new list
            forM_ existingRelays $ \r ->
                when (getUri r `notElem` map getUri relays) $
                    disconnectRelay $ getUri r

            importFn pk relays ts
            updateRelays (AccountId $ pubKeyXOToBech32 pk) (relays, ts)

            -- Connect to new relays that weren't in the old list
            let newRelays = filter (\r -> getUri r `notElem` map getUri existingRelays) relays
            forM_ newRelays $ \r -> async $ do
                connected <- connectRelay $ getUri r
                when connected $ handleRelaySubscription $ getUri r
        Nothing -> do
            importFn pk relays ts
            updateRelays (AccountId $ pubKeyXOToBech32 pk) (relays, ts)
            forM_ relays $ \r -> async $ do
                connected <- connectRelay $ getUri r
                when connected $ handleRelaySubscription $ getUri r
        _ -> pure ()


-- | Create DM relay subscription filters
createDMRelayFilters :: PubKeyXO -> [PubKeyXO] -> [Filter]
createDMRelayFilters xo followedPubKeys =
    [ --NT.metadataFilter (xo : followedPubKeys)
    --, NT.preferredDMRelaysFilter (xo : followedPubKeys)
    NT.giftWrapFilter xo
    ]


-- | Create inbox relay subscription filters
createInboxRelayFilters :: PubKeyXO -> [PubKeyXO] -> [Filter]
createInboxRelayFilters xo followedPubKeys =
    {-
    [ NT.followListFilter (xo : followedPubKeys)
    , NT.metadataFilter (xo : followedPubKeys)
    , NT.shortTextNoteFilter (xo : followedPubKeys)
    , NT.preferredDMRelaysFilter (xo : followedPubKeys)
    ]
    -}
    [ Filter
        { ids = Nothing
        , authors = Just (xo : followedPubKeys)
        , kinds = Just [FollowList, Metadata, ShortTextNote, EventDeletion, Repost, PreferredDMRelays]
        , since = Nothing
        , NT.until = Nothing
        , limit = Just 1000
        , fTags = Nothing
        } ]

-- | Generate a random subscription ID
generateRandomSubscriptionId :: SubscriptionEff es => Eff es SubscriptionId
generateRandomSubscriptionId = do
    bytes <- liftIO $ replicateM 8 randomIO
    let byteString = BS.pack bytes
    return $ decodeUtf8 $ B16.encode byteString
