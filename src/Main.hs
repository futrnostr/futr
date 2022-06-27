{-# LANGUAGE OverloadedStrings    #-}

module Main where

import Debug.Trace

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad (forever, mzero, void)
import Control.Monad.STM (atomically)
import Crypto.Schnorr (XOnlyPubKey)
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.List (filter, find, nub, sort, sortBy)
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, pack, strip)
import Monomer
import Monomer.Widgets.Single
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist)

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map

import AppTypes
import Futr
import Helpers
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayConnection
import Nostr.RelayPool
import Nostr.Request as Request
import Nostr.Response
import UI
import UIHelpers

import qualified Widgets.BackupKeys as BackupKeys
import qualified Widgets.EditProfile as EditProfile
import qualified Widgets.KeyManagement as KeyManagement
import qualified Widgets.PostDetails as PostDetails
import qualified Widgets.RelayManagement as RelayManagement
import qualified Widgets.ViewPosts as ViewPosts
import qualified Widgets.ViewProfile as ViewProfile

main :: IO ()
main = do
  relays <- loadRelaysFromDisk
  pool <- newMVar $ RelayPool relays Map.empty
  request <- atomically newBroadcastTChan
  startApp def (handleEvent $ AppEnv pool request) (UI.buildUI pool request) config
  where
    config =
      [ appWindowTitle "futr - nostr client"
      , appTheme customDarkTheme
      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      , appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf"
      , appInitEvent AppInit
      , appDisposeEvent Dispose
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
  --case trace (show evt) evt of
  case evt of
    NoOp -> []
    AppInit ->
      [ Task loadKeysFromDisk
      , Producer $ initRelays $ env ^. pool
      , Producer $ timerLoop
      ]
    RelaysInitialized rs ->
      [ Model $ model
          & relays .~ rs
          & waitingForConns .~ not (or (map connected rs))
      , case model ^. futr . selectedKeys of
          Just ks -> Monomer.Event InitSubscriptions
          Nothing -> Monomer.Event NoOp
      ]
    TimerTick now ->
      [ Model $ updateFutr model newFutr ]
      where
        newFutr = model ^. futr & time .~ now
    -- subscriptions
    InitSubscriptions ->
      [ Producer $ loadContacts (env ^. pool) (env ^. request) model ]
    SubscriptionsInitialized cs ->
      [ Model $ newModel
          & subscriptionId .~ Nothing
      , Producer $ initSubscriptions (env ^. pool) (env ^. request) (fromJust $ newFutr ^. selectedKeys) (Map.keys cs)
      ]
      where
        newFutr = model ^. futr
          & contacts .~ Map.keys cs
          & profiles .~ cs
        newModel = updateFutr model newFutr
    SubscriptionStarted subId ->
      [ Model $ model & subscriptionId .~ Just subId ]
    NewResponses responseList ->
      [ Model $ updateFutr model (newFutr (model ^. futr) responseList) ]
    Dispose ->
      [ voidTask $ closeSubscriptions (env ^. pool) (env ^. request) (model ^. subscriptionId) ]
    -- actions
    SendPost ->
      [ Model $ model & inputField .~ ""
      , voidTask $ sendPost (env ^. request) (model ^. futr) (model ^. inputField)
      ]
    ViewPostDetails re ->
      [ Model $ model
          & currentView .~ PostDetailsView
          & postDetailsModel . PostDetails.event .~ Just re
      ]
    ViewProfile xo' ->
      [ Model $ model
          & viewProfileModel . ViewProfile.profile .~ Just xo'
          & currentView .~ ProfileView
      ]
      where
        ((Profile name displayName about pictureUrl), _) = fromMaybe (def, fromSeconds 0) (Map.lookup xo' (model ^. futr . profiles))
    Follow xo ->
      [ Model $ updateFutr model newFutr
      , voidTask $ saveContacts (env ^. request) (fromJust $ model ^. futr . selectedKeys) (map (\c -> (c, Nothing)) newContacts)
      ]
      where
        newContacts = xo : (nub $ model ^. futr . contacts)
        newFutr = model ^. futr & contacts .~ newContacts
    Unfollow xo ->
      [ Model $ updateFutr model newFutr
      , voidTask $ saveContacts (env ^. request) (fromJust $ model ^. futr . selectedKeys) (map (\c -> (c, Nothing)) newContacts)
      ]
      where
        newContacts = filter (\xo' -> xo /= xo') (model ^. futr . contacts)
        newFutr = model ^. futr & contacts .~ newContacts
    -- go to
    GoHome ->
      [ Model $ model & currentView .~ HomeView ]
    GoKeyManagement ->
      [ Model $ model
          & currentView .~ KeyManagementView
          & keyMgmtModel . KeyManagement.keyList .~ model ^. keys
          & keyMgmtModel . KeyManagement.kmProfiles .~ model ^. futr . profiles
      ]
    AppTypes.GoSetup ->
      [ Model $ model
          & currentView .~ SetupView
          & setupModel .~ def
      ]
    GoRelayManagement ->
      [ Model $ model
          & currentView .~ RelayManagementView
          & relayMgmtModel . RelayManagement.rmRelays .~ model ^. relays
      ]
    KeyPairsLoaded ks ->
      [ Model $ newModel
          & keys .~ verifyActiveKeys ks
          & currentView .~ HomeView
      , Task $ saveKeyPairs ks (verifyActiveKeys ks)
      ]
      where
        mk = mainKeys $ verifyActiveKeys ks
        (Keys _ xo _ _) = mk
        newFutr = model ^. futr & selectedKeys .~ Just mk
        newModel = updateFutr model newFutr
    NoKeysFound ->
      [ Model $ model & currentView .~ SetupView ]
    ErrorReadingKeysFile ->
      [ Model $ model & errorMsg .~ (Just $ pack "Could not read keys file.\nCheck the file permissions. Maybe the file was corrupted.") ]
    NewKeysCreated ks profile datetime ->
      [ Model $ newModel
          & keys .~ ks : dk
          & AppTypes.backupKeysModel . BackupKeys.backupKeys .~ Just ks
          & currentView .~ BackupKeysView
      , Task $ saveKeyPairs (model ^. keys) (ks : dk)
      , Monomer.Event InitSubscriptions
      ]
      where
        dk = disableKeys $ model ^. keys
        Profile _ _ _ picture = profile
        Keys _ xo _ _ = ks
        newFutr = model ^. futr
          & profiles .~ Map.insert xo (profile, datetime) (model ^. futr . profiles)
          & selectedKeys .~ Just ks
        newModel = updateFutr model newFutr
    KeysBackupDone ->
      [ Model $ model
          & currentView .~ HomeView
      ]
    KeysUpdated keysList ->
      [ Model $ newModel
          & keys .~ keysList
      , Task $ saveKeyPairs (model ^. keys) keysList
      , if null keysList then Model $ model & currentView .~ SetupView else Monomer.Event InitSubscriptions
      ]
      where
        ks = if null keysList then initialKeys else head $ filter (\(Keys _ _ active _) -> active == True) keysList
        newFutr = def { _selectedKeys = Just ks}
        newModel = updateFutr model newFutr

    -- relays
    ConnectRelay relay ->
      [ Producer $ connectRelay env relay ]
    AppTypes.RelaysUpdated rs ->
      [ Model $ model
          & relays .~ rs
          & relayMgmtModel . RelayManagement.rmRelays .~ rs
          & waitingForConns .~ not (or (map connected rs))
      ]
    -- edit profile
    EditProfile ->
      [ Model $ model
          & currentView .~ EditProfileView
          & editProfileModel . EditProfile.nameInput .~ name
          & editProfileModel . EditProfile.displayNameInput .~ fromMaybe "" displayName
          & editProfileModel . EditProfile.aboutInput .~ fromMaybe "" about
          & editProfileModel . EditProfile.pictureInput .~ fromMaybe "" picture
          & editProfileModel . EditProfile.currentImage .~ fromMaybe "" pic
      ]
      where
        Keys _ xo _ _ = fromJust $ model ^. futr . selectedKeys
        Profile name displayName about picture = fst $ fromMaybe (def, fromSeconds 0)
          $ Map.lookup xo (model ^. futr . profiles)
        pic = do
          ((Profile _ _ _ picture), _) <- Map.lookup xo (model ^. futr . profiles)
          p <- picture
          return p
    ProfileUpdated ks profile datetime ->
      [ Model $ newModel & keys .~ ks' : newKeyList
      , Task $ saveKeyPairs (model ^. keys) (ks' : newKeyList)
      ]
      where
        Profile name displayName about picture = profile
        (Keys pk xo active _) = ks
        ks' = Keys pk xo active (Just name)
        newKeyList = filter (\k -> not $ k `sameKeys` ks') (model ^. keys)
        newFutr = model ^. futr
          & selectedKeys .~ (
            if ks `sameKeys` (fromJust $ model ^. futr . selectedKeys)
              then Just ks'
              else (model ^. futr . selectedKeys)
            )
          & profiles .~ case Map.lookup xo (model ^. futr . profiles) of
            Nothing ->
              Map.insert xo (profile, datetime) (model ^. futr . profiles)
            Just (profile', datetime') ->
              if datetime > datetime'
                then Map.insert xo (profile', datetime) (model ^. futr . profiles)
                else model ^. futr . profiles
        newModel = updateFutr model newFutr

loadKeysFromDisk :: IO AppEvent
loadKeysFromDisk = do
  let fp = "keys.ft"
  fe <- doesFileExist fp
  if not fe then return NoKeysFound
  else do
    content <- LazyBytes.readFile fp
    case decode content :: Maybe [Keys] of
      Just [] ->
        return NoKeysFound
      Just ks ->
        return $ KeyPairsLoaded ks
      _       ->
        return ErrorReadingKeysFile

loadRelaysFromDisk :: IO [Relay]
loadRelaysFromDisk = do
  let fp = "relays.ft"
  fe <- doesFileExist fp
  if not fe then return defaultRelays
  else do
    content <- LazyBytes.readFile fp
    case decode content :: Maybe [Relay] of
      Just [] ->
        return defaultRelays
      Just relays ->
        return relays
      _       ->
        return defaultRelays

initRelays :: MVar RelayPool -> (AppEvent -> IO ()) -> IO ()
initRelays pool sendMsg = do
  (RelayPool relays _) <- readMVar pool
  mapM_ (\relay -> sendMsg $ ConnectRelay relay) relays
  waitForActiveConnections  pool (3 * (10 ^ 6)) -- wait 3 secs to get some initial connections
  (RelayPool relays' _) <- readMVar pool
  sendMsg $ RelaysInitialized relays'

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env relay sendMsg = do
  connect (env ^. pool) (env ^. request) sendMsg RelaysUpdated relay

mainKeys :: [Keys] -> Keys
mainKeys ks = head $ filter (\(Keys _ _ xo _) -> xo == True) ks

saveKeyPairs :: [Keys] -> [Keys] -> IO AppEvent
saveKeyPairs oldKeys newKeys =
  if sort oldKeys == sort newKeys
    then return NoOp
    else do
      LazyBytes.writeFile "keys.ft" $ encode newKeys
      putStrLn "KeyPairs saved to disk"
      return NoOp

timerLoop :: (AppEvent -> IO ()) -> IO ()
timerLoop sendMsg = void $ forever $ do
  now <- getCurrentTime
  sendMsg $ TimerTick now
  threadDelay $ 500 * 1000

-- subscriptions

closeSubscriptions :: MVar RelayPool -> TChan Request -> Maybe SubscriptionId -> IO ()
closeSubscriptions pool request subId = do
  case subId of
    Just subId' ->
      unsubscribe pool request subId'
    Nothing ->
      return ()

initSubscriptions
  :: MVar RelayPool
  -> TChan Request
  -> Keys
  -> [XOnlyPubKey]
  -> (AppEvent -> IO ())
  -> IO ()
initSubscriptions pool request (Keys _ xo _ _) contacts sendMsg = do
  now <- getCurrentTime
  let initialFilters = [ MetadataFilter contacts now, TextNoteFilter contacts now, AllMetadata now ]
  response <- atomically newTChan
  subId <- subscribe pool request response initialFilters
  sendMsg $ SubscriptionStarted subId
  void . forever $ do
    msg <- atomically $ readTChan response
    msgs <- collectJustM . atomically $ tryReadTChan response
    sendMsg $ NewResponses (msg : msgs)
    threadDelay $ 100 * 1000 -- to avoid re-rendering, we only send 10 times per second new data in batches to the UI

loadContacts
  :: MVar RelayPool
  -> TChan Request
  -> AppModel
  -> (AppEvent -> IO ())
  -> IO ()
loadContacts pool request model sendMsg = do
  if not $ null $ model ^. futr . contacts
  then return ()
  else do
    now <- getCurrentTime
    response <- atomically newTChan
    subId <- subscribe pool request response [ ContactsFilter [ xo ] now ]
    msg <- atomically $ readTChan response
    case msg of
      (EventReceived _ event, _) -> do
        case kind event of
          Contacts -> do
            unsubscribe pool request subId
            let contacts = Map.fromList $ catMaybes $ map (tagToProfile $ created_at event) (tags event)
            sendMsg $ SubscriptionsInitialized contacts
          _ -> putStrLn "Unexpected event kind received when loading contacts" -- @todo handle differently
      _ -> mzero
  where
    (Keys _ xo _ _) = fromJust $ model ^. futr . selectedKeys

sendPost :: TChan Request -> FutrModel -> Text -> IO ()
sendPost request model post = do
  case model ^. selectedKeys of
    Nothing ->
      putStrLn "Cannot post message, so keys available"
    Just (Keys kp xo _ _) -> do
      now <- getCurrentTime
      let unsigned = textNote (strip post) xo now;
      atomically $ writeTChan request $ SendEvent $ signEvent unsigned kp xo

saveContacts :: TChan Request -> Keys -> [(XOnlyPubKey, Maybe Username)] -> IO ()
saveContacts request (Keys kp xo _ _) contacts = do
  now <- getCurrentTime
  let unsigned = setContacts contacts xo now
  atomically $ writeTChan request $ SendEvent $ signEvent unsigned kp xo

updateFutr :: AppModel -> FutrModel -> AppModel
updateFutr model new =
  model
    & futr .~ new
    & viewProfileModel . ViewProfile.futr .~ new
    & postDetailsModel . PostDetails.futr .~ new