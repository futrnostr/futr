{-# LANGUAGE RankNTypes #-}

-- | Module: Nostr.Relay
-- Relay connection and subscription management for Nostr.
--
-- Responsibilities:
--   * WebSocket connect/disconnect to relays
--   * Non-blocking subscriptions with per-subscription callbacks
--     (Subscribe / SubscribeTemporary) that return SubscriptionId immediately
--   * WaitForCompletion effect for blocking until subscription finishes
--   * Event routing (EventReceived) to the appropriate callback
--   * Handling EOSE/Closed and unsubscribing/cleanup
--   * Sending events to relays (SendEventToRelays)
--   * Tracking per-relay stats (successes/errors/bytes/eose/lastSeen)
--   * Treating relay-side signals (Notice/Auth)

module Nostr.Relay where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, replicateM, void, when)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effectful
import Effectful.Concurrent (Concurrent, forkIO, threadDelay)
import Effectful.Concurrent.Async (Async, async, race)
import Effectful.Concurrent.STM ( TChan, TMVar, TVar, atomically, newTChanIO
                                , newEmptyTMVarIO, putTMVar, readTChan
                                , takeTMVar, writeTChan
                                , newTVarIO, readTVar, writeTVar, retry )
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO, send)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Network.URI (URI(..), parseURI, uriAuthority, uriPort, uriRegName, uriScheme)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection.PingPong (defaultPingPongOptions, withPingPong)
import Prelude hiding (until)
import System.Random (randomIO)
import Text.Read (readMaybe)
import Wuss qualified as Wuss

import Nostr (Nostr, signEvent)
import Nostr.Event (Event(..), EventId, createCanonicalAuthentication)
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Types ( Filter, RelayURI, Request, Response(..), SubscriptionId
                   , normalizeRelayURI )
import Nostr.Types qualified as NT
import Nostr.Util
import Logging (Logging, logWarning, logDebug)
import QtQuick (QtQuick, notifyRelayStatus)
import Store.Lmdb (LmdbStore, RelayStats(..), updateRelayStats)
import Types (AppState(..), PublishStatus)


-- | RelayConnection Effect API
data RelayConnection :: Effect where
  -- connect / disconnect
  Connect :: RelayURI -> RelayConnection m Bool
  Disconnect :: RelayURI -> RelayConnection m ()
  -- subscriptions
  Subscribe :: RelayURI -> Filter -> (Event -> RelayURI -> m ()) -> RelayConnection m SubscriptionId
  SubscribeTemporary :: RelayURI -> Filter -> (Event -> RelayURI -> m ()) -> RelayConnection m SubscriptionId
  Unsubscribe :: SubscriptionId -> RelayConnection m ()
  UnsubscribeAll :: RelayURI -> RelayConnection m ()
  -- waiting
  WaitForCompletion :: SubscriptionId -> RelayConnection m ()
  -- send event
  SendEventToRelays :: Event -> [RelayURI] -> RelayConnection m ()


type instance DispatchOf RelayConnection = Dynamic


-- | State for RelayPool handling.
data RelayPool = RelayPool
    { activeConnections :: Map RelayURI RelayData
    , subscriptions :: Map SubscriptionId SubscriptionState
    , publishStatus :: Map EventId (Map RelayURI PublishStatus)
    , reconciliationThread :: Maybe (Async ())
    , commentSubscriptions :: Map EventId [SubscriptionId]
    , subscriptionCallbacks :: Map SubscriptionId (Event -> RelayURI -> IO ())
    , subscriptionCompletion :: Map SubscriptionId (TVar Bool)
    }


-- | Subscription state.
data SubscriptionState = SubscriptionState
    { relay :: RelayURI
    , subscriptionFilter :: Filter
    , eventsSeen :: Int
    , latestCreatedAtSeen :: Int
    , eoseSeen :: Bool
    , startedAt :: Int
    , lastActivityTs :: Int
    , isTemporary :: Bool
    }


-- | Create a new subscription state.
newSubscriptionState :: RelayURI -> Filter -> SubscriptionState
newSubscriptionState r f = SubscriptionState r f 0 0 False 0 0 False


-- | Connection errors.
data ConnectionError
    = ConnectionFailed Text
    | AuthenticationFailed Text
    | NetworkError Text
    | TimeoutError
    | InvalidRelayConfig
    | MaxRetriesReached
    | UserDisconnected
    deriving (Show, Eq)


-- | Relay connection state.
data ConnectionState = Connected | Connecting
  deriving (Show, Eq)


-- | Data for each relay.
data RelayData = RelayData
  { connectionState :: ConnectionState
  , requestChannel :: TChan Request
  , notices        :: [Text]
  , lastError      :: Maybe ConnectionError
  , connectionAttempts :: Int
  , pendingRequests :: [Request]
  , pendingEvents :: [Event]
  , pendingAuthId :: Maybe EventId
  , lastConnectedTs :: Int
  }


-- | Initial state for RelayPool.
initialRelayPool :: RelayPool
initialRelayPool = RelayPool
  { activeConnections = Map.empty
  , subscriptions = Map.empty
  , publishStatus = Map.empty
  , reconciliationThread = Nothing
  , commentSubscriptions = Map.empty
  , subscriptionCallbacks = Map.empty
  , subscriptionCompletion = Map.empty
  }


connect :: RelayConnection :> es => RelayURI -> Eff es Bool
connect = send . Connect

disconnect :: RelayConnection :> es => RelayURI -> Eff es ()
disconnect = send . Disconnect

subscribe :: RelayConnection :> es => RelayURI -> Filter -> (Event -> RelayURI -> Eff es ()) -> Eff es SubscriptionId
subscribe r f cb = send $ Subscribe r f cb

subscribeTemporary :: RelayConnection :> es => RelayURI -> Filter -> (Event -> RelayURI -> Eff es ()) -> Eff es SubscriptionId
subscribeTemporary r f cb = send $ SubscribeTemporary r f cb

unsubscribe :: RelayConnection :> es => SubscriptionId -> Eff es ()
unsubscribe = send . Unsubscribe

unsubscribeAll :: RelayConnection :> es => RelayURI -> Eff es ()
unsubscribeAll = send . UnsubscribeAll

sendEventToRelays :: RelayConnection :> es => Event -> [RelayURI] -> Eff es ()
sendEventToRelays ev rs = send $ SendEventToRelays ev rs

waitForCompletion :: RelayConnection :> es => SubscriptionId -> Eff es ()
waitForCompletion = send . WaitForCompletion


type RelayConnectionEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
  , Nostr :> es
  , Concurrent :> es
  , QtQuick :> es
  , Util :> es
  , Logging :> es
  , IOE :> es
  )


runRelayConnection :: RelayConnectionEff es => Eff (RelayConnection : es) a -> Eff es a
runRelayConnection = interpret $ \env -> \case
  Connect r -> do
    let r' = normalizeRelayURI r
    conns <- gets @RelayPool activeConnections
    if Map.member r' conns
      then do
        case connectionState <$> Map.lookup r' conns of
          Just Connected -> pure True
          Just Connecting -> do
            -- Wait for the in-flight connect attempt to complete (up to ~5s)
            let waitForConnection = do
                  stWait <- get @RelayPool
                  case Map.lookup r' (activeConnections stWait) of
                    Just rdWait -> case connectionState rdWait of
                      Connected    -> pure True
                      Connecting   -> do
                        threadDelay 100000  -- 100ms delay
                        waitForConnection
                    Nothing -> pure False
            timeoutResult <- race waitForConnection (threadDelay 5000000 >> pure False)
            case timeoutResult of
              Left connected -> pure connected
              Right _ -> pure False  -- Timeout
          _ -> do
            chan <- newTChanIO
            establishConnection r' chan
      else do
        chan <- newTChanIO
        now <- getCurrentTime
        let rd = RelayData
                  { connectionState = Connecting
                  , requestChannel = chan
                  , lastError = Nothing
                  , connectionAttempts = 0
                  , notices = []
                  , pendingRequests = []
                  , pendingEvents = []
                  , pendingAuthId = Nothing
                  , lastConnectedTs = now
                  }
        modify @RelayPool $ \st -> st { activeConnections = Map.insert r' rd (activeConnections st) }
        establishConnection r' chan

  Disconnect r -> do
    let r' = normalizeRelayURI r
    st <- get @RelayPool
    for_ (Map.lookup r' (activeConnections st)) $ \rd -> do
      void $ atomically $ writeTChan (requestChannel rd) NT.Disconnect
      modify @RelayPool $ \st' -> st' { activeConnections = Map.delete r' (activeConnections st') }
      now <- getCurrentTime
      updateRelayStats r' (\s -> s { disconnects = disconnects s + 1, lastSeenTs = now })

  Subscribe r f cb -> do
    subId <- generateRandomSubscriptionId
    now <- getCurrentTime
    let r' = normalizeRelayURI r
    st <- get @RelayPool
    case Map.lookup r' (activeConnections st) of
      Just rd -> do
        let stNew = (newSubscriptionState r' f) { startedAt = now, isTemporary = False }
        (cb' :: Event -> RelayURI -> IO ()) <- localUnliftIO env (ConcUnlift Persistent Unlimited) $ \unlift ->
            pure $ \e r'' -> unlift (cb e r'')
        done <- newTVarIO False
        modify @RelayPool $ \s ->
          s { subscriptions = Map.insert subId stNew (subscriptions s)
            , subscriptionCallbacks = Map.insert subId cb' (subscriptionCallbacks s) }
        modify @RelayPool $ \s -> s { subscriptionCompletion = Map.insert subId done (subscriptionCompletion s) }
        atomically $ writeTChan (requestChannel rd) (NT.Subscribe $ NT.Subscription subId f)
        return subId
      Nothing -> error $ "Subscribe: Relay not connected: " ++ T.unpack r'

  SubscribeTemporary r f cb -> do
    subId <- generateRandomSubscriptionId
    now <- getCurrentTime
    let r' = normalizeRelayURI r
    st <- get @RelayPool
    case Map.lookup r' (activeConnections st) of
      Just rd -> do
        let tempState = (newSubscriptionState r' f) { eoseSeen = False, startedAt = now, isTemporary = True }
        (cb' :: Event -> RelayURI -> IO ()) <- localUnliftIO env (ConcUnlift Persistent Unlimited) $ \unlift ->
            pure $ \e r'' -> unlift (cb e r'')
        done <- newTVarIO False
        modify @RelayPool $ \s ->
          s { subscriptions = Map.insert subId tempState (subscriptions s)
            , subscriptionCallbacks = Map.insert subId cb' (subscriptionCallbacks s) }
        modify @RelayPool $ \s -> s { subscriptionCompletion = Map.insert subId done (subscriptionCompletion s) }
        atomically $ writeTChan (requestChannel rd) (NT.Subscribe $ NT.Subscription subId f)
        return subId
      Nothing -> error $ "SubscribeTemporary: Relay not connected: " ++ T.unpack r'

  Unsubscribe subId -> do
    st <- get @RelayPool
    for_ (Map.lookup subId (subscriptions st)) $ \sd -> do
      let r' = relay sd
      for_ (Map.lookup r' (activeConnections st)) $ \rd ->
        atomically $ writeTChan (requestChannel rd) (NT.Close subId)
      for_ (Map.lookup subId (subscriptionCompletion st)) $ \tv -> atomically $ writeTVar tv True
      modify @RelayPool $ \s -> 
        s { subscriptions = Map.delete subId (subscriptions s)
          , subscriptionCallbacks = Map.delete subId (subscriptionCallbacks s)
          , subscriptionCompletion = Map.delete subId (subscriptionCompletion s) }

  UnsubscribeAll r -> do
    let r' = normalizeRelayURI r
    st <- get @RelayPool
    let subIds = [ sid | (sid, sd) <- Map.toList (subscriptions st), relay sd == r' ]
    for_ (Map.lookup r' (activeConnections st)) $ \rd ->
      forM_ subIds $ \sid -> atomically $ writeTChan (requestChannel rd) (NT.Close sid)
    forM_ subIds $ \sid ->
      for_ (Map.lookup sid (subscriptionCompletion st)) $ \tv -> atomically $ writeTVar tv True
    modify @RelayPool $ \s -> s
      { subscriptions = foldr Map.delete (subscriptions s) subIds
      , subscriptionCallbacks = foldr Map.delete (subscriptionCallbacks s) subIds
      , subscriptionCompletion = foldr Map.delete (subscriptionCompletion s) subIds
      }

  SendEventToRelays ev relays -> do
    st <- get @RelayPool
    forM_ relays $ \r ->
      for_ (Map.lookup (normalizeRelayURI r) (activeConnections st)) $ \rd ->
        atomically $ writeTChan (requestChannel rd) (NT.SendEvent ev)

  WaitForCompletion subId -> do
    st <- get @RelayPool
    case Map.lookup subId (subscriptionCompletion st) of
      Nothing -> pure ()
      Just tv -> do
        -- Add timeout to prevent infinite blocking
        --logDebug $ "Wait start: sid=" <> subId
        timeoutResult <- race
          (let loop = do
                  done <- atomically $ readTVar tv
                  if done
                    then pure ()
                    else do
                      threadDelay 1000000
                      loop
            in loop) 
          (threadDelay 20000000 >> pure ()) -- 20 second timeout
        case timeoutResult of
          Left () -> do
            --logDebug $ "Wait done: sid=" <> subId
            pure () -- Completed normally
          Right () -> do
            -- Timeout occurred, log warning and continue
            --logWarning $ "Subscription " <> pack (show subId) <> " timed out after 20 seconds"
            pure ()


-- | Establish a connection to a relay.
establishConnection :: RelayConnectionEff es => RelayURI -> TChan NT.Request -> Eff es Bool
establishConnection r requestChan = do
  modify @RelayPool $ \st' -> st' { activeConnections = Map.adjust (\d -> d { connectionState = Connecting, connectionAttempts = connectionAttempts d + 1 }) r (activeConnections st') }
  connectionMVar <- newEmptyTMVarIO
  let connectAction = case parseURI (T.unpack r) of
        Just uri -> case uriAuthority uri of
          Just auth ->
            let host = uriRegName auth
                portInt = case uriPort auth of
                             (':':rest) | not (null rest) -> maybe defaultPort id (readMaybe rest)
                             _ -> defaultPort
                defaultPort = if uriScheme uri == "wss:" then (443 :: Int) else 80
                path = let p = uriPath uri in if null p then "/" else p
            in if uriScheme uri == "wss:" then Wuss.runSecureClient host (fromIntegral portInt) path else WS.runClient host (fromIntegral portInt) path
          Nothing -> error $ "Invalid relay URI (no authority): " ++ T.unpack r
        Nothing -> error $ "Invalid relay URI: " ++ T.unpack r
  void $ forkIO $ withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    let runClient = nostrClient connectionMVar r requestChan runE
    result <- try @SomeException $ connectAction runClient
    case result of
      Right _ -> pure ()
      Left e -> runE $ do
        atomically $ putTMVar connectionMVar False
        now <- getCurrentTime
        updateRelayStats r (\s -> s { errorsCount = errorsCount s + 1
                                    , disconnects = disconnects s + 1
                                    , lastFailureTs = now
                                    , lastSeenTs = now })
        st' <- get @RelayPool
        when (Map.member r (activeConnections st')) $ modify @RelayPool $ \s ->
          s { activeConnections = Map.delete r (activeConnections s) }
  atomically $ takeTMVar connectionMVar


-- | Run the Nostr client.
nostrClient :: RelayConnectionEff es => TMVar Bool -> RelayURI -> TChan NT.Request -> (forall a. Eff es a -> IO a) -> WS.ClientApp ()
nostrClient connectionMVar r requestChan runE conn = runE $ do
    let conn' = conn
  --liftIO $ withPingPong defaultPingPongOptions conn $ \conn' -> runE $ do
    now <- getCurrentTime
    modify @RelayPool $ \st -> st { activeConnections = Map.adjust (\d -> 
        d { connectionState = Connected
          , requestChannel = requestChan
          , lastConnectedTs = now
          }) r (activeConnections st) }
    updateRelayStats r (\s -> s { successes = successes s + 1, lastSeenTs = now })
    void $ atomically $ putTMVar connectionMVar True
    notifyRelayStatus
    void $ async $ receiveLoop conn'
    sendLoop conn'
    -- Connection is closing; purge any subscriptions tied to this relay to avoid lingering state
    stSubs <- get @RelayPool
    let subIdsToPurge = [ sid | (sid, sd) <- Map.toList (subscriptions stSubs), relay sd == r ]
    for_ subIdsToPurge $ \sid' ->
      for_ (Map.lookup sid' (subscriptionCompletion stSubs)) $ \tv -> atomically $ writeTVar tv True
    modify @RelayPool $ \s -> s
      { subscriptions = foldr Map.delete (subscriptions s) subIdsToPurge
      , subscriptionCallbacks = foldr Map.delete (subscriptionCallbacks s) subIdsToPurge
      , subscriptionCompletion = foldr Map.delete (subscriptionCompletion s) subIdsToPurge
      }
    modify @RelayPool $ \st' -> st' { activeConnections = Map.delete r (activeConnections st') }
    notifyRelayStatus
  where
    receiveLoop conn' = do
      --logDebug $ "WS recv loop: " <> r
      msg <- liftIO (try (WS.receiveData conn') :: IO (Either SomeException ByteString))
      case msg of
        Left err -> do
          --logDebug $ "WS recv error: " <> r <> ": " <> pack (show err)
          now <- getCurrentTime
          updateRelayStats r (\s -> s { errorsCount = errorsCount s + 1
                                      , disconnects = disconnects s + 1
                                      , lastFailureTs = now
                                      , lastSeenTs = now })
          modify @RelayPool $ \st -> st { activeConnections = Map.adjust (\d -> d { lastError = Just (NetworkError "recv error") }) r (activeConnections st) }
        Right msg' -> do
          now <- getCurrentTime
          let rawSnippet = T.take 250 $ decodeUtf8 $ BS.take 250 msg'
          --logDebug $ "WS recv: bytes=" <> pack (show (BSL.length msg')) <> " raw: " <> r <> " " <> rawSnippet
          updateRelayStats r (\s -> s { bytesRxTotal = bytesRxTotal s + fromIntegral (BS.length msg')
                                      , lastSeenTs = now })
          case eitherDecodeStrict msg' of
            Right response -> do
              handleResponse r response
              -- case response of
              --   EventReceived subId' _ -> logDebug $ "Decoded EVENT: " <> r <> " sid=" <> subId'
              --   Eose subId'            -> logDebug $ "Decoded EOSE: "  <> r <> " sid=" <> subId'
              --   Closed subId' _        -> logDebug $ "Decoded CLOSED: " <> r <> " sid=" <> subId'
              --   Notice _               -> logDebug $ "Decoded NOTICE: " <> r
              --   Auth _                 -> logDebug $ "Decoded AUTH: "   <> r
              --   Ok _ acc _             -> logDebug $ "Decoded OK: "     <> r <> " accepted=" <> pack (show acc)
              case response of
                EventReceived subId' event' -> do
                  stCb <- get @RelayPool
                  for_ (Map.lookup subId' (subscriptionCallbacks stCb)) $ \cb -> liftIO $ cb event' r
                _ -> pure ()
              receiveLoop conn'
            Left _ -> do
              --logWarning $ "WS decode error: " <> r <> " raw=" <> rawSnippet
              updateRelayStats r (\s -> s { errorsCount = errorsCount s + 1
                                          , lastFailureTs = now
                                          , lastSeenTs = now })
              modify @RelayPool $ \st -> st { activeConnections = Map.adjust (\d -> d { lastError = Just (NetworkError "decode error") }) r (activeConnections st) }
              receiveLoop conn'
    sendLoop conn' = do
      msg <- atomically $ readTChan requestChan
      case msg of
        NT.Disconnect -> do
          liftIO $ WS.sendClose conn' (T.pack "Bye!")
          pure ()
        NT.SendEvent _ -> do
          let out = encode msg
          --logDebug $ "WS send: " <> r <> " bytes=" <> pack (show (BSL.length out))
          res <- liftIO $ try @SomeException $ WS.sendTextData conn' out
          case res of
            Right _ -> do
              now <- getCurrentTime
              updateRelayStats r (\s -> s { bytesTxTotal = bytesTxTotal s + fromIntegral (BSL.length out)
                                          , lastSeenTs = now })
              sendLoop conn'
            Left _ -> do
              now <- getCurrentTime
              updateRelayStats r (\s -> s { errorsCount = errorsCount s + 1
                                          , lastFailureTs = now
                                          , lastSeenTs = now })
              sendLoop conn'
        _ -> do
          let out = encode msg
          res <- liftIO $ try @SomeException $ WS.sendTextData conn' out
          case res of
            Right _ -> do
              now <- getCurrentTime
              updateRelayStats r (\s -> s { bytesTxTotal = bytesTxTotal s + fromIntegral (BSL.length out)
                                          , lastSeenTs = now })
              sendLoop conn'
            Left _ -> do
              now <- getCurrentTime
              updateRelayStats r (\s -> s { errorsCount = errorsCount s + 1
                                          , lastFailureTs = now
                                          , lastSeenTs = now })
              sendLoop conn'


-- | Handle a response from a relay.
handleResponse :: RelayConnectionEff es => RelayURI -> Response -> Eff es ()
handleResponse relayURI' r = case r of
  EventReceived subId' event' -> do
    now <- getCurrentTime
    modify @RelayPool $ \st ->
      st { subscriptions = Map.adjust (\sd -> sd
        { latestCreatedAtSeen = max (latestCreatedAtSeen sd) (createdAt event')
        , eventsSeen = eventsSeen sd + 1, lastActivityTs = now
        }) subId' (subscriptions st) }
    updateRelayStats relayURI' (\s -> s { eventsSeenTotal = eventsSeenTotal s + 1
                                        , latestEventCreatedAtSeen = max (latestEventCreatedAtSeen s) (createdAt event')
                                        , lastSeenTs = now })

  Eose subId' -> do
    now <- getCurrentTime
    st <- get @RelayPool
    for_ (Map.lookup subId' (subscriptions st)) $ \sd -> do
      if isTemporary sd
        then do
          let r' = relay sd
          for_ (Map.lookup r' (activeConnections st)) $ \rd -> do
            atomically $ writeTChan (requestChannel rd) (NT.Close subId')
          for_ (Map.lookup subId' (subscriptionCompletion st)) $ \tv -> do
            atomically $ writeTVar tv True
          modify @RelayPool $ \s -> s { subscriptions = Map.delete subId' (subscriptions s)
                                       , subscriptionCallbacks = Map.delete subId' (subscriptionCallbacks s)
                                       , subscriptionCompletion = Map.delete subId' (subscriptionCompletion s) }
        else do
          modify @RelayPool $ \s ->
            s { subscriptions = Map.adjust (\x -> x 
              { eoseSeen = True
              , lastActivityTs = now
              }) subId' (subscriptions s) }
          --logDebug $ "EOSE handled: " <> relayURI' <> " sid=" <> subId'
    updateRelayStats relayURI' (\s -> s { lastEoseTs = now, lastSeenTs = now })

  Closed subId' _ -> do
    now <- getCurrentTime
    st <- get @RelayPool
    for_ (Map.lookup subId' (subscriptionCompletion st)) $ \tv -> do
      atomically $ writeTVar tv True
    modify @RelayPool $ \s -> s { subscriptions = Map.delete subId' (subscriptions s)
                                 , subscriptionCallbacks = Map.delete subId' (subscriptionCallbacks s)
                                 , subscriptionCompletion = Map.delete subId' (subscriptionCompletion s) }
    updateRelayStats relayURI' (\s -> s { lastSeenTs = now })

  Ok _ accepted _ -> do
    now <- getCurrentTime
    if accepted
      then updateRelayStats relayURI' (\s -> s { successes = successes s + 1, lastSeenTs = now })
      else updateRelayStats relayURI' (\s -> s { errorsCount = errorsCount s + 1, lastFailureTs = now, lastSeenTs = now })

  Notice msg -> do
    now <- getCurrentTime
    modify @RelayPool $ \st -> st
      { activeConnections = Map.adjust (\rd -> rd { notices = take 20 (msg : notices rd) }) relayURI' (activeConnections st)
      , subscriptions = Map.map (\sd -> if relay sd == relayURI' then sd { lastActivityTs = now } else sd) (subscriptions st)
      }
    updateRelayStats relayURI' (\s -> s { noticesCount = noticesCount s + 1, lastSeenTs = now })

  Auth challenge -> do
    st <- get @RelayPool
    for_ (Map.lookup relayURI' (activeConnections st)) $ \rd -> do
      now <- getCurrentTime
      updateRelayStats relayURI' (\s -> s { authRequired = authRequired s + 1, lastSeenTs = now })
      kp <- getKeyPair
      let unsignedEvent = createCanonicalAuthentication relayURI' challenge (keyPairToPubKeyXO kp) now
      signedEventMaybe <- signEvent unsignedEvent kp
      for_ signedEventMaybe $ \signedEvent -> do
        modify @RelayPool $ \st' -> st'
          { activeConnections = Map.adjust (\rd' -> rd' { pendingAuthId = Just (eventId signedEvent) }) relayURI' (activeConnections st')
          , subscriptions = Map.map (\sd -> if relay sd == relayURI' then sd { lastActivityTs = now } else sd) (subscriptions st')
          }
        atomically $ writeTChan (requestChannel rd) (NT.Authenticate signedEvent)


-- | Generate a random subscription ID
generateRandomSubscriptionId :: RelayConnectionEff es => Eff es SubscriptionId
generateRandomSubscriptionId = do
    bytes <- liftIO $ replicateM 8 randomIO
    let byteString = BS.pack bytes
    return $ decodeUtf8 $ B16.encode byteString
