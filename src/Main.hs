{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                    as Exception
import           Control.Lens
import           Control.Monad                        (forever, mzero, unless, void)
import           Control.Monad.STM                    (atomically)
import           Control.Monad.Trans                  (lift, liftIO)
import           Control.Monad.Trans.Maybe            (runMaybeT)
import           Crypto.Schnorr                       (XOnlyPubKey, KeyPair,
                                                       decodeHex, deriveXOnlyPubKey,
                                                       keyPairFromSecKey, generateKeyPair,
                                                       secKey, xOnlyPubKey)
import           Data.Aeson
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.DateTime
import           Data.Default
import           Data.Either                          (fromRight)
import           Data.List                            (find, sort, sortBy)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe
import           Data.Monoid                          (mconcat)
import           Data.Text                            (Text, strip)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Monomer
import           Monomer.Widgets.Single
import qualified Network.Connection                   as Connection
import qualified Network.HTTP.Req                     as Req
--import           Network.Socket
import           Network.WebSockets                   (ClientApp, Connection,
                                                       receiveData, sendClose,
                                                       sendTextData)
import qualified Network.WebSockets                   as WS
import qualified Network.WebSockets.Stream            as Stream
import           System.Directory                     (doesFileExist)
import qualified Text.URI                             as URI
import           Text.URI.Lens
import           Wuss

import           AppTypes
import           Helpers
import           Nostr.Event
import           Nostr.Filter
import           Nostr.Keys
import           Nostr.Kind
import           Nostr.Profile  as Profile
import           Nostr.Relay
import           Nostr.RelayConnection
import           Nostr.RelayPool
import           Nostr.Request  as Request
import           Nostr.Response
import           UI
import           UIHelpers
import qualified Widgets.EditProfile                  as EditProfile
import qualified Widgets.Home                         as Home
import qualified Widgets.ViewProfile                  as ViewProfile
import qualified Widgets.ViewPosts                    as ViewPosts

import           Debug.Trace as Trace
import qualified Crypto.Hash.SHA256     as SHA256 -- debug only

main :: IO ()
main = do
  channel <- atomically newBroadcastTChan
  poolMVar <- newMVar def
  startApp def (handleEvent $ AppEnv channel poolMVar) (buildUI channel poolMVar) config
  where
    config =
      [ appWindowTitle "FuTr"
      , appTheme customDarkTheme
      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      , appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf"
      , appInitEvent AppInit
      -- , appDisableAutoScale True
      ]

handleEvent
  :: AppEnv
  -> AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent env wenv node model evt =
  case evt of
    NoOp -> []
    ConnectRelay relay ->
      [ Producer $ connectRelay env relay
      ]
    DisconnectRelay r ->
      [ Task $ disconnectRelay env r
      , Model $ model & dialog .~ Nothing
      ]
    UpdateRelay r -> []
    AppInit ->
      [ Task loadKeysFromDisk
      , Producer $ initRelays env
      ]
    RelayConnected r ->
      [ Model $ model & relays .~ r : (removeRelayFromList (model ^. relays) r)
      ]
    RelayDisconnected r ->
      [ Model $ model & relays .~ r : (removeRelayFromList (model ^. relays) r)
      ]
    ValidateAndAddRelay ->
      [ Task $ validateAndAddRelay (model ^. relayModel) ]
    InvalidRelayURI ->
      [ Model $ model & relayModel . isInvalidInput .~ True ]
    AddRelay relay ->
      [ Producer $ connectRelay env relay
      , Model $ model
          & relays .~ relay : (removeRelayFromList (model ^. relays) relay)
      ]
    ShowDialog d ->
      case d of
        RelayDialog r ->
          [ Model $ model
            & dialog .~ Just (RelayDialog r)
            & relayModel . relayReadableInput .~ readable info'
            & relayModel . relayWritableInput .~ writable info'
          ]
          where
            info' = info r
        NewRelayDialog ->
          [ Model $ model & dialog .~ Just NewRelayDialog ]
        _ -> []
    KeyPairsLoaded ks ->
      [ Model $ model
        & keys .~ ks
        & selectedKeys .~ Just mk
        & currentView .~ HomeView
      ]
      where
        mk = mainKeys ks
        (Keys _ xo _ _) = mk
    NoKeysFound ->
      [ Model $ model & currentView .~ SetupView ]
    ErrorReadingKeysFile ->
      [ Model $ model & dialog .~ Just ErrorReadingKeysFileDialog ]
    CloseDialog ->
      [ Model $ model & dialog .~ Nothing ]
    -- relay connection
    TimerTick now ->
      [ Model $ model & homeModel . Home.time .~ now ]
    Initialize ->
      [ Task $ sendInitFilter (env ^. channel) (fromJust $ model ^. selectedKeys)
      , Producer timerLoop
      ]
    InitSubscribed subId ->
      [ Model $ model & homeModel . Home.initSub .~ subId ]
    EventAppeared e r -> do
      let newModel = Home.handleReceivedEvent (model ^. homeModel) e r
      if not $ model ^. homeModel . Home.isInitialized
        && model ^. homeModel . Home.isInitialized
        then
          [ Model $ model & homeModel .~ newModel
          , Task $ sendHomeFilters (env ^. channel) model
          ]
        else
          [ Model $ model & homeModel .~ newModel ]
    HomeFilterSubscribed subId ->
      [ Model $ model & homeModel . Home.homeSub .~ subId ]

initRelays :: AppEnv -> (AppEvent -> IO ()) -> IO ()
initRelays env sendMsg = do
  (RelayPool relays _) <- readMVar $ env ^. relayPool
  mapM_ (\relay -> sendMsg $ AddRelay relay) relays

-- runSearchProfile :: Text -> IO AppEvent
-- runSearchProfile v = do
--   case maybe Nothing xOnlyPubKey $ decodeHex v of
--     Just xo ->
--       return $ ViewProfile xo
--     _ ->
--       return NoOp

-- handleDeleteEvent :: AppEnv -> Event -> AppModel -> IO AppEvent
-- handleDeleteEvent env event model = do
--   now <- getCurrentTime
--   let (Keys kp xo _ _) = fromJust $ model ^. selectedKeys
--   let unsigned = deleteEvents [eventId event] (model ^. deleteReason) xo now
--   atomically $ writeTChan (env ^. channel) $ SendEvent $ signEvent unsigned kp xo
--   return EventSent

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env relay sendMsg =
  connect (env ^. channel) (env ^. relayPool) sendMsg RelayConnected RelayDisconnected relay

{-
connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env r sendMsg = if connected r then return () else do
  putStrLn $ "trying to connect to " ++ (T.unpack $ relayName r) ++ " ..."
  start $ \conn -> do
    let r' = r { connected = True }
    putStrLn $ "Connected to " ++ (T.unpack $ relayName r)
    sendMsg $ RelayConnected r'
    if readable info' then receiveWs r' conn sendMsg else sendMsg NoOp
    if writable info' then sendWs (env ^. channel) r' conn sendMsg else sendMsg NoOp
  where
    host = T.unpack $ extractHostname r
    port = extractPort r
    path = T.unpack $ extractPath r
    info' = info r
    start = case extractScheme r of
      "wss" -> runSecureClient host (fromIntegral port) path
      "ws"  -> WS.runClient host port path
      _     -> error "Wrong websocket scheme"
-}

disconnectRelay :: AppEnv -> Relay -> IO AppEvent
disconnectRelay env r = if not $ connected r then return NoOp else do
  atomically $ writeTChan (env ^. channel) $ Disconnect r
  return NoOp

receiveWs :: Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
receiveWs r conn sendMsg = void . forkIO $ void . runMaybeT $ forever $ do
  msg <- lift (Exception.try $ WS.receiveData conn :: IO (Either WS.ConnectionException LazyBytes.ByteString))
  case msg of
    Left ex    -> do
      liftIO $ putStrLn $ "Connection to " ++ (T.unpack $ relayName r) ++ " closed"
      lift $ sendMsg $ RelayDisconnected r
      mzero
    Right msg' -> case decode msg' of
      Just m -> case m of
          EventReceived subId event ->
            lift $ sendMsg $ EventAppeared event r
          Notice notice ->
            lift $ putStrLn $ "NOTICE: " ++ T.unpack notice
      Nothing -> do
        lift $ putStrLn $ "Could not decode server response: " ++ show msg'
        lift $ sendMsg $ NoOp

sendWs :: TChan Request -> Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
sendWs broadcastChannel r conn sendMsg = do
  channel <- atomically $ dupTChan broadcastChannel
  forever $ do
    msg <- Exception.try $ liftIO . atomically $ readTChan channel :: IO (Either WS.ConnectionException Request)
    case msg of
      Left ex -> sendMsg $ RelayDisconnected r
      Right msg' -> case msg' of
        Disconnect r' ->
          if r `sameRelay` r' then do
              WS.sendClose conn $ T.pack "Bye!"
          else return ()
        _ ->
          WS.sendTextData conn $ encode msg'

saveKeyPairs :: [Keys] -> IO AppEvent
saveKeyPairs ks = do
  LazyBytes.writeFile "keys.ft" $ encode ks
  putStrLn "KeyPairs saved to disk"
  return NoOp

loadKeysFromDisk :: IO AppEvent
loadKeysFromDisk = do
  let fp = "keys.ft"
  fe <- doesFileExist fp
  if not fe then return NoKeysFound
  else do
    content <- LazyBytes.readFile fp
    case decode content :: Maybe [Keys] of
      Just [] -> do
        return NoKeysFound
      Just ks -> do
        return $ KeyPairsLoaded ks
      _       -> do
        return ErrorReadingKeysFile

validateAndAddRelay :: RelayModel -> IO AppEvent
validateAndAddRelay model = do
  uri <- URI.mkURI $ model ^. relayURI
  return $ if isValidRelayURI uri
    then do
      let info = RelayInfo (model ^. relayReadableInput) (model ^. relayWritableInput)
      AddRelay (Relay uri info False)
    else
      InvalidRelayURI

switchEnabledKeys :: Keys -> [Keys] -> [Keys]
switchEnabledKeys (Keys kp _ _ _) ks =
  map (\(Keys kp' xo' a' n') -> if kp == kp'
    then Keys kp' xo' True n'
    else Keys kp' xo' False n'
  ) ks

timerLoop :: (AppEvent -> IO ()) -> IO ()
timerLoop sendMsg = void . forkIO $ void $ forever $ do
  now <- getCurrentTime
  sendMsg $ TimerTick now
  threadDelay 1000000

sendInitFilter :: TChan Request -> Keys -> IO AppEvent
sendInitFilter channel (Keys _ xo _ _) = do
  --subId <- subscribe channel [InitialFilter xo]
  return $ InitSubscribed "dumy" -- subId

sendHomeFilters :: TChan Request -> AppModel -> IO AppEvent
sendHomeFilters channel model = do
  --subId <- subscribe channel [TextNoteFilter xoList, ContactsFilter xoList]
  return $ HomeFilterSubscribed "dummy" -- subId
  where
    (Keys _ xo _ _) = fromJust $ model ^. homeModel . Home.keys
    contactList = map extractXOFromProfile (model ^. homeModel . Home.contacts)
    xoList = xo : contactList

mainKeys :: [Keys] -> Keys
mainKeys ks = head $ filter (\(Keys _ _ xo _) -> xo == True) ks
