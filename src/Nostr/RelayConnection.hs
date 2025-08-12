{-# LANGUAGE BlockArguments #-}

module Nostr.RelayConnection where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when, unless)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, forkIO, threadDelay)
import Effectful.Concurrent.Async (async, waitAnyCancel)
import Effectful.Concurrent.STM ( TChan, TMVar, atomically, newTChanIO
                                , newEmptyTMVarIO, putTMVar, readTChan
                                , takeTMVar, writeTChan )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets, modify)
import qualified Nostr.RelayPool as RelayPool
import Network.URI (URI(..), parseURI, uriAuthority, uriPort, uriRegName)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection.PingPong (defaultPingPongOptions, withPingPong)
import Prelude hiding (until)
import Wuss qualified as Wuss

import QtQuick
import Logging
import Nostr
import Nostr.Event (Event(..), Kind(..), createCanonicalAuthentication)
import Nostr.EventHandler (EventHandler, handleEvent)
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Relay (RelayURI, normalizeRelayURI)
import Nostr.FilterSet (filtersStructurallyChanged)
import Nostr.Types (Filter(..), Response(..), SubscriptionId)
import Nostr.Types qualified as NT
import Nostr.Util
import Types ( AppState(..), ConnectionError(..), ConnectionState(..)
             , PublishStatus(..), queueMax)
import Nostr.RelayPool (RelayPool, RelayPoolState(..), RelayData(..), SubscriptionState(..), RelayStats(..))
import Store.Lmdb (LmdbStore, putRelayStats, getRelayStats, putFeedAnchor)
import Nostr.FeedKeys (feedKeyForPosts, feedKeyForMentionsHex, feedKeyForComments)


-- | Reason for disconnecting from a relay.
data DisconnectReason = UserInitiated | ConnectionFailure
    deriving (Show, Eq)


-- | Effect for handling RelayPool operations.
data RelayConnection :: Effect where
    Connect :: RelayURI -> RelayConnection m Bool
    Disconnect :: RelayURI -> RelayConnection m ()


type instance DispatchOf RelayConnection = Dynamic


connect :: RelayConnection :> es => RelayURI -> Eff es Bool
connect uri = send $ Connect uri

disconnect :: RelayConnection :> es => RelayURI -> Eff es ()
disconnect uri = send $ Disconnect uri


-- | RelayConnectionEff
type RelayConnectionEff es =
  ( State AppState :> es
  , State RelayPool.RelayPoolState :> es
  , RelayPool.RelayPool :> es
  , Nostr :> es
  , EventHandler :> es
  , LmdbStore :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  , Util :> es
  , IOE :> es
  )


-- | Handler for relay pool effects.
runRelayConnection
  :: RelayConnectionEff es
  => Eff (RelayConnection : es) a
  -> Eff es a
runRelayConnection = interpret $ \_ -> \case
    Connect r -> do
        let r' = normalizeRelayURI r
        conns <- RelayPool.getActiveConnectionsMap
        if Map.member r' conns
            then do
                case Map.lookup r' conns of
                  Nothing -> pure False
                  Just rd -> do
                    avoided <- isAvoidedNow rd
                    if avoided
                      then pure False
                      else case connectionState rd of
                             Connected    -> pure True
                             Connecting   -> pure False
                             Disconnected -> do
                               chan <- newTChanIO
                               connectWithRetry r' defaultMaxRetries chan
             else do
                chan <- newTChanIO
                mStats <- getRelayStats r'
                let rd = initializeRelayData chan mStats
                RelayPool.insertActiveConnection r' rd
                RelayPool.clearExcludedRelay r'
                connectWithRetry r' defaultMaxRetries chan

    Disconnect r -> do
        let r' = normalizeRelayURI r
        st <- get @RelayPool.RelayPoolState
        case Map.lookup r' (activeConnections st) of
            Just rd -> do
                void $ atomically $ writeTChan (requestChannel rd) NT.Disconnect
                RelayPool.adjustActiveConnection r' (\d -> d { connectionState = Disconnected, pendingUserDisconnect = True })
            Nothing -> return ()


-- | True if the relay is currently in its avoid window.
isAvoidedNow :: RelayConnectionEff es => RelayData -> Eff es Bool
isAvoidedNow rd = do
  nowTs <- getCurrentTime
  pure $ maybe False (> nowTs) (avoidUntil rd)

-- | Default number of retries for new or reconnecting connections.
defaultMaxRetries :: Int
defaultMaxRetries = 5

-- | Initialize a 'RelayData' from an optional persisted 'RelayStats'.
initializeRelayData :: TChan NT.Request -> Maybe RelayStats -> RelayData
initializeRelayData chan mStats =
  let (suc0, fail0, lcon0, leose0, avoid0) = case mStats of
        Just st -> ( rsSuccessCount st
                   , rsFailureCount st
                   , rsLastConnectedAt st
                   , rsLastEoseAt st
                   , rsAvoidUntil st )
        Nothing -> (0, 0, Nothing, Nothing, Nothing)
  in RelayData
       { connectionState = Connecting
       , requestChannel = chan
       , lastError = Nothing
       , connectionAttempts = 0
       , notices = []
       , pendingRequests = []
       , pendingEvents = []
       , queuedRequests = []
       , pendingAuthId = Nothing
       , recentFailure = False
       , successCount = suc0
       , failureCount = fail0
       , lastConnectedAt = lcon0
       , lastEoseAt = leose0
       , messagesReceived = 0
       , eventsReceived = 0
       , authRequiredCount = 0
       , noticeCount = 0
       , avoidUntil = avoid0
       , pendingUserDisconnect = False
       }

-- | Connect with retry.
connectWithRetry :: RelayConnectionEff es => RelayURI -> Int -> TChan NT.Request -> Eff es Bool
connectWithRetry r maxRetries requestChan = do
    st <- get @RelayPool.RelayPoolState

    let attempts = maybe 0 connectionAttempts $ Map.lookup r (activeConnections st)
    if attempts >= maxRetries
        then do
            RelayPool.adjustActiveConnection r (\d -> d { connectionState = Disconnected
                                                        , lastError = Just "MaxRetriesReached"
                                                        , failureCount = failureCount d + 1
                                                        ,  recentFailure = True })
            applyAvoidPenalty r
            return False
        else do
            -- Add exponential backoff delay
            when (attempts > 0) $ do
                let delayMs = min 5000000 (1000000 * (2 ^ attempts))  -- Cap at 5 seconds
                threadDelay delayMs

            RelayPool.adjustActiveConnection r (\d -> d { connectionState = Connecting
                                                        , connectionAttempts = connectionAttempts d + 1 })

            connectionMVar <- newEmptyTMVarIO

            let connectAction = case parseURI (T.unpack r) of
                    Just uri -> case uriAuthority uri of
                        Just auth ->
                            let host = uriRegName auth
                                port = case uriPort auth of
                                    "" -> if "wss://" `T.isPrefixOf` r then 443 else 80
                                    p -> read (drop 1 p) -- drop the leading ':'
                            in if "wss://" `T.isPrefixOf` r
                                then Wuss.runSecureClient host port "/"
                                else WS.runClient host (fromIntegral port) "/"
                        Nothing -> error $ "Invalid relay URI (no authority): " ++ T.unpack r
                    Nothing -> error $ "Invalid relay URI: " ++ T.unpack r

            void $ forkIO $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
                let runClient = nostrClient connectionMVar r requestChan runE
                result <- try @SomeException $ connectAction runClient
                case result of
                    Right _ -> return ()
                    Left e -> runE $ do
                        --logError $ "Connection failed for relay " <> r <> ": " <> T.pack (show e)
                        atomically $ putTMVar connectionMVar False
                        isActive <- RelayPool.isActiveConnection r
                        when isActive $
                            RelayPool.adjustActiveConnection r (\d -> d { connectionState = Disconnected
                                                                        , lastError = Just $ ("Connection failed: " <> T.pack (show e))
                                                                        , failureCount = failureCount d + 1
                                                                        , recentFailure = True })
                        applyAvoidPenalty r

            atomically $ takeTMVar connectionMVar


-- | Nostr client for relay connections.
nostrClient :: RelayConnectionEff es => TMVar Bool -> RelayURI -> TChan NT.Request -> (forall a. Eff es a -> IO a) -> WS.ClientApp ()
nostrClient connectionMVar r requestChan runE conn = runE $ do
    liftIO $ withPingPong defaultPingPongOptions conn $ \conn' -> runE $ do
        RelayPool.adjustActiveConnection r (\d -> d { connectionState = Connected
                        , requestChannel = requestChan
                                                    , successCount = successCount d + 1 })
        nowTs <- getCurrentTime
        RelayPool.setLastConnectedAt r nowTs
        void $ atomically $ putTMVar connectionMVar True
        notifyRelayStatus

        -- Drain any queued requests accumulated while disconnected (FIFO). These are connectivity buffers only.
        -- AUTH gating uses pendingRequests/pendingEvents separately and will be released on AUTH success.
        stQueued <- get @RelayPool.RelayPoolState
        case Map.lookup r (activeConnections stQueued) of
          Just rdq -> do
            forM_ (reverse $ queuedRequests rdq) $ \req ->
              atomically $ writeTChan requestChan req
            RelayPool.adjustActiveConnection r (\d -> d { queuedRequests = [] })
          Nothing -> pure ()

        st <- get @RelayPool.RelayPoolState
        let allPending = pendingSubscriptions st
        let forThisRelay = Map.filter (\d -> relay d == r) allPending
        let remainingPending = Map.filter (\d -> relay d /= r) allPending
        when (not (Map.null forThisRelay)) $
          logDebug $ "Starting " <> T.pack (show (Map.size forThisRelay)) <> " pending subs on " <> r
        forM_ (Map.toList forThisRelay) $ \(subId', details) -> do
            let f = subscriptionFilter details
            atomically $ writeTChan requestChan (NT.Subscribe $ NT.Subscription subId' f)
            RelayPool.adjustSubscription subId' (\x -> x { lastSentFilter = Just f })
        RelayPool.mergeSubscriptions forThisRelay
        RelayPool.setPendingSubscriptions remainingPending

        -- Re-subscribe existing subscriptions for this relay after reconnect
        stResub <- get @RelayPool.RelayPoolState
        let existingForRelay = [ (sid, sd)
                               | (sid, sd) <- Map.toList (subscriptions stResub)
                               , relay sd == r
                               , Map.notMember sid forThisRelay -- avoid double-send
                               ]
        when (not (null existingForRelay)) $
          logDebug $ "Resubscribing " <> T.pack (show (length existingForRelay)) <> " subs on " <> r
        forM_ existingForRelay $ \(sid, sd) -> do
          -- Diff: if only since moved forward, avoid resend
          let oldF = lastSentFilter sd
              newF = subscriptionFilter sd
          case oldF of
            Just ofilt | filtersStructurallyChanged ofilt newF -> do
              atomically $ writeTChan requestChan (NT.Subscribe $ NT.Subscription sid newF)
              RelayPool.adjustSubscription sid (\x -> x { lastSentFilter = Just newF })
            _ -> pure ()

        receiveThread <- async $ receiveLoop conn'
        sendThread <- async $ sendLoop conn'
        void $ waitAnyCancel [receiveThread, sendThread]
        -- If this was not a user-initiated disconnect, count as failure and penalize briefly
        stEnd <- get @RelayPool.RelayPoolState
        case Map.lookup r (activeConnections stEnd) of
          Just rdEnd -> do
            unless (pendingUserDisconnect rdEnd || recentFailure rdEnd) $ do
              RelayPool.incrementFailure r
              applyAvoidPenalty r
            -- clear flags after handling
            RelayPool.adjustActiveConnection r (\d -> d { pendingUserDisconnect = False
                                                       , recentFailure = False })
          Nothing -> pure ()
        RelayPool.adjustActiveConnection r (\d -> d { connectionState = Disconnected })
        -- Add a short penalty box in excludedRelays for picker to consider
        nowTsBox <- getCurrentTime
        RelayPool.setExcludedRelayUntil r (nowTsBox + 120)
        notifyRelayStatus
  where
    receiveLoop conn' = do
        msg <- liftIO (try (WS.receiveData conn') :: IO (Either SomeException BSL.ByteString))
        case msg of
            Left _ -> return ()  -- Exit the loop on error
            Right msg' -> case eitherDecode msg' of
                Right response -> do
                    RelayPool.incrementMessagesReceived r
                    updates <- handleResponse r response
                    notify updates
                    persistRelayStatsIfNeeded r
                    receiveLoop conn'
                Left _ -> do
                    --logError $ "Could not decode server response from " <> r <> ": " <> T.pack err
                    --logError $ "Msg: " <> T.pack (show msg')
                    receiveLoop conn'

    sendLoop conn' = do
        msg <- atomically $ readTChan requestChan
        case msg of
            NT.Disconnect -> do
                liftIO $ WS.sendClose conn' (T.pack "Bye!")
                return ()
            NT.SendEvent event -> do
                result <- liftIO $ try @SomeException $ WS.sendTextData conn' $ encode msg
                case result of
                    Left _ -> pure ()
                    Right _ -> do
                        trackPendingEvents r event
                        sendLoop conn'
            _ -> do
                result <- liftIO $ try @SomeException $ WS.sendTextData conn' $ encode msg
                case result of
                    Left _ -> pure ()
                        --logError $ "Error sending data to " <> r <> ": " <> T.pack (show ex)
                    Right _ -> sendLoop conn'

    -- Persist relay stats periodically (every ~100 messages)
    persistRelayStatsIfNeeded :: RelayConnectionEff es => RelayURI -> Eff es ()
    persistRelayStatsIfNeeded relay = do
      pool <- get @RelayPool.RelayPoolState
      case Map.lookup relay (activeConnections pool) of
        Just rd | messagesReceived rd `mod` 100 == 0 ->
          persistRelayStatsForRelay relay
        _ -> pure ()

    -- Persist current in-memory stats for a relay
    persistRelayStatsForRelay :: RelayConnectionEff es => RelayURI -> Eff es ()
    persistRelayStatsForRelay relay = do
      pool <- get @RelayPool.RelayPoolState
      case Map.lookup relay (activeConnections pool) of
        Just rd -> do
          let stats = RelayStats
                { rsSuccessCount = successCount rd
                , rsFailureCount = failureCount rd
                , rsLastConnectedAt = lastConnectedAt rd
                , rsLastEoseAt = lastEoseAt rd
                , rsAvoidUntil = avoidUntil rd
                }
          putRelayStats relay stats
        Nothing -> pure ()

    -- Track pending event so we can retry after auth if required
    trackPendingEvents :: RelayConnectionEff es => RelayURI -> Event -> Eff es ()
    trackPendingEvents relay evt =
      RelayPool.adjustActiveConnection relay (\rd' -> rd' { pendingEvents = evt : pendingEvents rd' })


-- | Compute exponential backoff (seconds) bounded to a reasonable range.
computeBackoffSeconds :: Int -> Int
computeBackoffSeconds failures = min 600 (5 * (2 ^ min 6 failures))

-- | Apply avoid penalty to a relay based on current failure count.
applyAvoidPenalty :: RelayConnectionEff es => RelayURI -> Eff es ()
applyAvoidPenalty r = do
  nowTs <- getCurrentTime
  s2 <- RelayPool.getActiveConnectionsMap
  let failures = maybe 0 failureCount (Map.lookup r s2)
      backoff = computeBackoffSeconds failures
  RelayPool.setAvoidUntil r (nowTs + backoff)

-- | Handle responses.
handleResponse :: RelayConnectionEff es => RelayURI -> Response -> Eff es UIUpdates
handleResponse relayURI' r = case r of
    EventReceived subId' event' -> do
        st0 <- get @RelayPool.RelayPoolState
        let mFilter = subscriptionFilter <$> Map.lookup subId' (subscriptions st0)
        case mFilter of
          Just filt | not (eventMatchesFilter event' filt) -> do
            -- Ignore events that do not match our active filter (relay misbehavior or filter drift)
            return emptyUpdates
          _ -> do
            recordOldestCreatedAt subId' event'
            updateSubscriptionCounters subId'
            -- Immediately process the event
            handleEvent (Just relayURI') event'
            RelayPool.incrementEventsReceived relayURI'
            return emptyUpdates
        -- Debug: log when a subscription processes unexpectedly high number of events before EOSE
        let threshold = 10000 :: Int
        stDbg <- get @RelayPool.RelayPoolState
        case Map.lookup subId' (subscriptions stDbg) of
          Just sdbg -> when (eventsProcessed sdbg `mod` threshold == 0 && eventsProcessed sdbg > 0) $
                         logDebug $ "High eventsProcessed for sub " <> subId' <> " on " <> relayURI' <> ": " <> T.pack (show (eventsProcessed sdbg))
          Nothing -> pure ()
        return emptyUpdates
        where
            recordOldestCreatedAt :: RelayConnectionEff es => SubscriptionId -> Event -> Eff es ()
            recordOldestCreatedAt sid e = do
              RelayPool.adjustSubscription sid (\sd -> if createdAt e < oldestCreatedAt sd
                                                  then sd { oldestCreatedAt = createdAt e }
                                                  else sd)

            updateSubscriptionCounters :: RelayConnectionEff es => SubscriptionId -> Eff es ()
            updateSubscriptionCounters sid = RelayPool.adjustSubscription sid (\sd -> sd { eventsProcessed = eventsProcessed sd + 1 })

    Eose subId' -> do
        markSubEoseReceived subId'
        st <- get @RelayPool.RelayPoolState
        case Map.lookup subId' (subscriptions st) of
          Nothing -> pure ()
          Just subInfo -> do
            case Map.lookup (relay subInfo) (activeConnections st) of
              Nothing -> pure ()
              Just rd -> do
                currentTime <- getCurrentTime
                let channel = requestChannel rd
                handlePaginationOrClose channel subId' subInfo currentTime
                persistAnchorsForFilter subInfo
            nowTs <- getCurrentTime
            updateLastEoseTimestamp (relay subInfo) nowTs
            persistRelayStatsSnapshot (relay subInfo)
        return emptyUpdates

    Closed subId' msg -> do
        let prefix = T.takeWhile (/= ':') msg
        case () of
          _ | "auth-required" `T.isPrefixOf` msg -> do
                -- Cork and retry after authentication
                st <- get @RelayPool.RelayPoolState
                case Map.lookup subId' (subscriptions st) of
                  Just subDetails -> do
                    let subscription = NT.Subscription { NT.subId = subId', NT.filter = subscriptionFilter subDetails }
                    handleAuthRequired relayURI' (NT.Subscribe subscription)
                  Nothing -> pure ()
                return emptyUpdates

            | prefix == "rate-limited" -> do
                handleClosedRateLimited subId'
                return emptyUpdates

            | prefix == "duplicate" -> do
                -- Nothing to do; assume newer sub replaced prior
                return emptyUpdates

            | prefix == "invalid" || prefix == "restricted" || prefix == "error" -> do
                handleClosedInvalid subId'
                return emptyUpdates

            | otherwise -> do
                handleClosedDefaultRemove subId'
                return emptyUpdates

    Ok eventId' accepted' msg -> do
        let isAuthRequired = maybe False ("auth-required" `T.isPrefixOf`) msg

        when (not isAuthRequired && isJust eventId') $
            updatePublishStatusForEvent relayURI' eventId' accepted' msg

        if isAuthRequired
            then handleOkAuthRequired relayURI' eventId'
            else handleOkAuthSucceeded relayURI' eventId' accepted'

        return $ emptyUpdates { publishStatusChanged = True }

    Notice msg -> do
        updateNotices relayURI' msg
        return $ emptyUpdates { noticesChanged = True }

    Auth challenge -> do
        st <- get @RelayPool.RelayPoolState
        case Map.lookup relayURI' (activeConnections st) of
            Just rd -> do
                now <- getCurrentTime
                kp <- getKeyPair
                let unsignedEvent = createCanonicalAuthentication relayURI' challenge (keyPairToPubKeyXO kp) now
                signedEventMaybe <- signEvent unsignedEvent kp
                case signedEventMaybe of
                    Just signedEvent -> do
                        RelayPool.setPendingAuthId relayURI' (Just (eventId signedEvent))
                        atomically $ writeTChan (requestChannel rd) (NT.Authenticate signedEvent)
                        return emptyUpdates
                    Nothing -> do
                        --logError "Failed to sign canonical authentication event"
                        return emptyUpdates
            Nothing -> do
                --logError $ "Error handling relay authentication, no channel found: " <> relayURI'
                return emptyUpdates

    where
        -- Basic filter matcher (guards against relay-sent mismatches)
        eventMatchesFilter :: Event -> Filter -> Bool
        eventMatchesFilter ev f = and
            [ maybe True (\ks -> kind ev `elem` ks) (kinds f)
            , maybe True (\as -> pubKey ev `elem` as) (authors f)
            , maybe True (\ids' -> eventId ev `elem` ids') (ids f)
            , tagsMatch (fTags f) (tags ev)
            ]

        tagsMatch :: Maybe (Map.Map Char [T.Text]) -> [[T.Text]] -> Bool
        tagsMatch Nothing _ = True
        tagsMatch (Just wanted) evTags =
            let findTag c = [vals | (t:vals) <- evTags, t == T.singleton c]
            in all (\(c, vals) -> case findTag c of
                                    [] -> False
                                    presentVals:_ -> any (`elem` presentVals) vals)
                    (Map.toList wanted)

        -- Local helpers extracted for readability
        persistAnchorsForFilter :: RelayConnectionEff es => SubscriptionState -> Eff es ()
        persistAnchorsForFilter subInfo = do
          let f = subscriptionFilter subInfo
          nowAnchor <- getCurrentTime
          when (isPostsLikeKinds (kinds f)) $
            case authors f of
              Just as | not (null as) -> putFeedAnchor (relay subInfo) (feedKeyForPosts as) nowAnchor
              _ -> pure ()
          case fTags f of
            Just tags -> do
              -- Mentions anchor by 'p' tags when kinds look like mentions/posts
              when (isPostsLikeKinds (kinds f) && Map.member 'p' tags) $ do
                forM_ (Map.findWithDefault [] 'p' tags) $ \hex ->
                  putFeedAnchor (relay subInfo) (feedKeyForMentionsHex hex) nowAnchor
              -- Comments anchor by 'e' tag root
              when (Map.member 'e' tags) $ do
                forM_ (Map.findWithDefault [] 'e' tags) $ \rootHex ->
                  putFeedAnchor (relay subInfo) (feedKeyForComments rootHex) nowAnchor
            Nothing -> pure ()

        isPostsLikeKinds :: Maybe [Kind] -> Bool
        isPostsLikeKinds mk = case mk of
          Just ks -> all (`elem` ks) [ShortTextNote, Repost, Comment, EventDeletion]
          Nothing -> False

        markSubEoseReceived :: RelayConnectionEff es => SubscriptionId -> Eff es ()
        markSubEoseReceived sid = RelayPool.setSubEoseReceived sid

        updateLastEoseTimestamp :: RelayConnectionEff es => RelayURI -> Int -> Eff es ()
        updateLastEoseTimestamp relay ts = RelayPool.setLastEoseAt relay ts

        persistRelayStatsSnapshot :: RelayConnectionEff es => RelayURI -> Eff es ()
        persistRelayStatsSnapshot relay = do
          pool <- get @RelayPool.RelayPoolState
          case Map.lookup relay (activeConnections pool) of
            Just rd -> do
              let stats = RelayStats
                    { rsSuccessCount = successCount rd
                    , rsFailureCount = failureCount rd
                    , rsLastConnectedAt = lastConnectedAt rd
                    , rsLastEoseAt = lastEoseAt rd
                    , rsAvoidUntil = avoidUntil rd
                    }
              putRelayStats relay stats
            Nothing -> pure ()

        handlePaginationOrClose :: RelayConnectionEff es => TChan NT.Request -> SubscriptionId -> SubscriptionState -> Int -> Eff es ()
        handlePaginationOrClose ch sid subInfo nowTs = do
          let f = subscriptionFilter subInfo
              requestedLimit = limit f
              seenEventsCount = eventsProcessed subInfo
              mNextUntil = if oldestCreatedAt subInfo == maxBound then Nothing else Just (oldestCreatedAt subInfo)
              reachedLimit = maybe False (\l -> seenEventsCount >= l) requestedLimit
          if reachedLimit && mNextUntil /= Nothing
            then paginateSubscription ch sid f mNextUntil nowTs
            else closeTemporarySubscription ch sid subInfo seenEventsCount

        paginateSubscription :: RelayConnectionEff es => TChan NT.Request -> SubscriptionId -> Filter -> Maybe Int -> Int -> Eff es ()
        paginateSubscription ch sid f mNextUntil nowTs = do
          let adjustedUntil = fmap (\n -> let capped = min n nowTs in if capped > 0 then capped - 1 else 0) mNextUntil
              newFilter = f { until = adjustedUntil }
          logDebug $ "Paginating sub " <> sid <> " on " <> relayURI' <>
                     ", nextUntil=" <> T.pack (show adjustedUntil)
          atomically $ writeTChan ch (NT.Close sid)
          threadDelay 20000
          RelayPool.adjustSubscription sid (\sd -> sd { subscriptionFilter = newFilter
                                                     , oldestCreatedAt = maxBound
                                                     , eventsProcessed = 0
                                                     , eoseReceived = False })
          atomically $ writeTChan ch (NT.Subscribe $ NT.Subscription sid newFilter)

        closeTemporarySubscription :: RelayConnectionEff es => TChan NT.Request -> SubscriptionId -> SubscriptionState -> Int -> Eff es ()
        closeTemporarySubscription ch sid subInfo seenEventsCount = do
          logDebug $ "EOSE: closing sub " <> sid <> " on " <> relayURI' <>
                     ", totalSeen=" <> T.pack (show seenEventsCount)
          when (temporary subInfo) $ do
            atomically $ writeTChan ch (NT.Close sid)
            RelayPool.deleteSubscriptionId sid

        scheduleRateLimitedResubscribe :: RelayConnectionEff es => SubscriptionId -> Eff es ()
        scheduleRateLimitedResubscribe sid = do
          st <- get @RelayPool.RelayPoolState
          case Map.lookup sid (subscriptions st) of
            Just subDetails ->
              case Map.lookup (relay subDetails) (activeConnections st) of
                Just rd -> do
                  let f = subscriptionFilter subDetails
                      ch = requestChannel rd
                  void $ forkIO $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
                    runE $ threadDelay (2 * 1000000)
                    runE $ atomically $ writeTChan ch (NT.Subscribe $ NT.Subscription sid f)
                Nothing -> pure ()
            Nothing -> pure ()

        handleClosedRateLimited :: RelayConnectionEff es => SubscriptionId -> Eff es ()
        handleClosedRateLimited = scheduleRateLimitedResubscribe

        handleClosedInvalid :: RelayConnectionEff es => SubscriptionId -> Eff es ()
        handleClosedInvalid = demeritRelayAndRemoveSub

        handleClosedDefaultRemove :: RelayConnectionEff es => SubscriptionId -> Eff es ()
        handleClosedDefaultRemove = removeSubIfPresent

        demeritRelayAndRemoveSub :: RelayConnectionEff es => SubscriptionId -> Eff es ()
        demeritRelayAndRemoveSub sid = do
          st <- get @RelayPool.RelayPoolState
          case Map.lookup sid (subscriptions st) of
            Just subDetails -> do
              RelayPool.deleteSubscriptionId sid
              RelayPool.incrementFailure (relay subDetails)
              applyAvoidPenalty (relay subDetails)
            Nothing -> pure ()

        removeSubIfPresent :: RelayConnectionEff es => SubscriptionId -> Eff es ()
        removeSubIfPresent sid = do
          st <- get @RelayPool.RelayPoolState
          case Map.lookup sid (subscriptions st) of
            Just _ -> RelayPool.deleteSubscriptionId sid
            Nothing -> pure ()

        updatePublishStatusForEvent relay maybeEid accepted msg =
          case maybeEid of
            Just eid -> RelayPool.upsertPublishStatus eid relay (if accepted then Success else Failure (fromMaybe "Rejected by relay" msg))
            Nothing -> pure ()

        handleOkAuthRequired relay maybeEid = do
          st <- get @RelayPool.RelayPoolState
          case Map.lookup relay (activeConnections st) of
            Just rd -> do
              RelayPool.adjustActiveConnection relay (\d -> d { authRequiredCount = authRequiredCount d + 1 })
              case maybeEid of
                Just eid -> case find (\e -> eventId e == eid) (pendingEvents rd) of
                  Just event -> handleAuthRequired relay (NT.SendEvent event)
                  Nothing -> pure ()
                Nothing -> pure ()
            Nothing -> pure ()

        handleOkAuthSucceeded relay maybeEid accepted = do
          st <- get @RelayPool.RelayPoolState
          case Map.lookup relay (activeConnections st) of
            Just rd ->
              case (pendingAuthId rd, maybeEid) of
                (Just authId, Just eid) | authId == eid && accepted -> do
                  let pendingReqs = pendingRequests rd
                      pendingEvts = pendingEvents rd
                  clearPendingAuthState relay
                  forM_ pendingEvts $ \evt -> atomically $ writeTChan (requestChannel rd) (NT.SendEvent evt)
                  forM_ pendingReqs $ \req -> atomically $ writeTChan (requestChannel rd) req
                _ -> pure ()
            Nothing -> pure ()

        clearPendingAuthState :: RelayConnectionEff es => RelayURI -> Eff es ()
        clearPendingAuthState relay =
          RelayPool.adjustActiveConnection relay (\rd' -> rd' { pendingRequests = []
                                                             , pendingEvents = []
                                                             , pendingAuthId = Nothing })

        updateNotices :: RelayConnectionEff es => RelayURI -> T.Text -> Eff es ()
        updateNotices relay noticeMsg = do
          let capNotices ns = take 500 ns
          RelayPool.adjustActiveConnection relay (\rd -> let ns' = capNotices (noticeMsg : notices rd)
                                                         in rd { notices = ns' })
          stn <- get @RelayPool.RelayPoolState
          case Map.lookup relay (activeConnections stn) of
            Just rd -> when (length (notices rd) `mod` 500 == 0) $
                          logDebug $ "Notices list size for " <> relay <> ": " <> T.pack (show (length (notices rd)))
            Nothing -> pure ()


-- | Handle authentication required.
handleAuthRequired :: RelayConnectionEff es => RelayURI -> NT.Request -> Eff es ()
handleAuthRequired relayURI' request = case request of
    NT.SendEvent evt -> do
        -- Cap pendingEvents to avoid unbounded growth under repeated AUTH-required
        RelayPool.adjustActiveConnection relayURI' (\rd' -> let capped = take queueMax (evt : pendingEvents rd')
                                                           in rd' { pendingEvents = capped })
    _ -> do
        -- Cap pendingRequests similarly
        RelayPool.adjustActiveConnection relayURI' (\rd' -> let capped = take queueMax (request : pendingRequests rd')
                                                           in rd' { pendingRequests = capped })
