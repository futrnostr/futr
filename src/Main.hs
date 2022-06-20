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
import Data.List (find, sort, sortBy)
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
import qualified Widgets.RelayManagement as RelayManagement
import qualified Widgets.ViewPosts as ViewPosts

main :: IO ()
main = do
  channel <- atomically newBroadcastTChan
  relays <- loadRelaysFromDisk
  pool <- newMVar $ RelayPool relays Map.empty
  startApp def (handleEvent $ AppEnv pool channel) (UI.buildUI pool channel) config
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
  case trace (show evt) evt of
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
      , Monomer.Event InitSubscriptions
      ]
    TimerTick now ->
      [ Model $ model & futr . time .~ now ]
    -- subscriptions
    InitSubscriptions ->
      [ Producer $ loadContacts (env ^. pool) (env ^. channel) model ]
    SubscriptionsInitialized cs ->
      [ Model $ model
          & futr . contacts .~ cs
          & subscriptionId .~ Nothing
      , Producer $ initSubscriptions (env ^. pool) (env ^. channel) (model ^. futr . selectedKeys) (Map.keys cs)
      ]
    SubscriptionStarted subId ->
      [ Model $ model & subscriptionId .~ Just subId ]
    ContactsReceived cs ->
      [ Model $ model
          & futr . contacts .~ updateContacts (model ^. futr . contacts) cs
      ]
    TextNoteReceived event relay ->
      [ Model $ model
          & futr . events .~ newEvents
      ]
      where
        newEvents = addEvent (model ^. futr .  events) event relay
    Dispose ->
      [ voidTask $ closeSubscriptions (env ^. pool) (env ^. channel) (model ^. subscriptionId) ]
    -- actions
    SendPost ->
      [ Model $ model
          & inputField .~ ""
      , voidTask $ sendPost (env ^. channel) (model ^. futr) (model ^. inputField)
      ]
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
      [ Model $ model
        & keys .~ verifyActiveKeys ks
        & futr . selectedKeys .~ mk
        & currentView .~ HomeView
      , Task $ saveKeyPairs ks (verifyActiveKeys ks)
      ]
      where
        mk = mainKeys $ verifyActiveKeys ks
        (Keys _ xo _ _) = mk
    NoKeysFound ->
      [ Model $ model & currentView .~ SetupView ]
    ErrorReadingKeysFile ->
      [ Model $ model & errorMsg .~ (Just $ pack "Could not read keys file.\nCheck the file permissions. Maybe the file was corrupted.") ]
    NewKeysCreated ks profile datetime ->
      [ Model $ model
          & keys .~ ks : dk
          & futr . profiles .~ Map.insert xo (profile, datetime) (model ^. futr . profiles)
          & futr . selectedKeys .~ ks
          & AppTypes.backupKeysModel . BackupKeys.backupKeys .~ ks
          & currentView .~ BackupKeysView
      , Task $ saveKeyPairs (model ^. keys) (ks : dk)
      ]
      where
        dk = disableKeys $ model ^. keys
        Profile _ _ _ picture = profile
        Keys _ xo _ _ = ks
    KeysBackupDone ->
      [ Model $ model
          & currentView .~ HomeView
      ]
    KeysUpdated keysList ->
      [ Model $ model
          & keys .~ keysList
          & futr . selectedKeys .~ ks
      , Task $ saveKeyPairs (model ^. keys) keysList
      , if null keysList then Model $ model & currentView .~ SetupView else Monomer.Event NoOp
      ]
      where
        ks = if null keysList then initialKeys else head $ filter (\(Keys _ _ active _) -> active == True) keysList
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
          & editProfileModel . EditProfile.epProfiles .~ model ^. futr . profiles
          & editProfileModel . EditProfile.currentImage .~ fromMaybe "" pic
      ]
      where
        Keys _ xo _ _ = model ^. futr . selectedKeys
        Profile name displayName about picture = fst $ fromMaybe (def, fromSeconds 0)
          $ Map.lookup xo (model ^. futr . profiles)
        pic = do
          ((Profile _ _ _ picture), _) <- Map.lookup xo (model ^. futr . profiles)
          p <- picture
          return p
    ProfileUpdated ks profile datetime ->
      [ Model $ model
          & keys .~ ks' : newKeyList
          & futr . selectedKeys .~ (
            if ks `sameKeys` (model ^. futr . selectedKeys)
              then ks'
              else (model ^. futr . selectedKeys)
            )
          & AppTypes.backupKeysModel . BackupKeys.backupKeys .~ (
            if ks `sameKeys` (model ^. futr . selectedKeys)
              then ks'
              else (model ^. futr . selectedKeys)
            )
          & futr . profiles .~
            case Map.lookup xo (model ^. futr . profiles) of
              Nothing ->
                Map.insert xo (profile, datetime) (model ^. futr . profiles)
              Just (profile', datetime') ->
                if datetime > datetime'
                  then Map.insert xo (profile', datetime) (model ^. futr . profiles)
                  else model ^. futr . profiles
      , Task $ saveKeyPairs (model ^. keys) (ks' : newKeyList)
      ]
      where
        Profile name displayName about picture = profile
        (Keys pk xo active _) = ks
        ks' = Keys pk xo active (Just name)
        newKeyList = filter (\k -> not $ k `sameKeys` ks') (model ^. keys)

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
connectRelay env relay sendMsg =
  connect (env ^. channel) (env ^. pool) sendMsg RelaysUpdated relay

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
  threadDelay 1000000

-- subscriptions

updateContacts :: Map XOnlyPubKey (Profile, DateTime) -> [(XOnlyPubKey, (Profile, DateTime))] -> Map XOnlyPubKey (Profile, DateTime)
updateContacts original new =
  Map.union newContacts original
  where
    newContacts = Map.fromList $ catMaybes $ map (checkContactIsNewer original) new

checkContactIsNewer :: Map XOnlyPubKey (Profile, DateTime) -> (XOnlyPubKey, (Profile, DateTime)) -> Maybe (XOnlyPubKey, (Profile, DateTime))
checkContactIsNewer original (xo, (p, d)) =
  case Map.lookup xo original of
    Nothing ->
      Just (xo, (p, d))
    Just (p', d') ->
      if d' > d
        then Just (xo, (p', d'))
        else Nothing

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
  response <- atomically newTChan
  subId <- subscribe pool request response initialFilters
  sendMsg $ SubscriptionStarted subId
  void . forever $ do
    msg <- atomically $ readTChan response
    case msg of
      (EventReceived _ event, relay) -> do
        case kind event of
          TextNote -> do
            sendMsg $ TextNoteReceived event relay
          Contacts -> do
            sendMsg $ ContactsReceived $ catMaybes $ map (tagToProfile $ created_at event) (tags event)
          Metadata -> do
            case parseProfiles event of
              Just p -> sendMsg $ ContactsReceived [ p ]
              Nothing -> return ()
          _ -> putStrLn "Unexpected event kind received" -- @todo handle differently

      _ -> putStrLn "Unexpected data received" -- @todo handle differently
  where
    initialFilters = [ MetadataFilter contacts, TextNoteFilter contacts ]
    parseProfiles e = case readProfile e of
      Just p -> Just (pubKey e, (p, created_at e))
      Nothing -> Nothing

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
    response <- atomically newTChan
    subId <- subscribe pool request response [ ContactsFilter [ xo ] ]
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
    (Keys _ xo _ _) = model ^. futr . selectedKeys

tagToProfile :: DateTime -> Tag -> Maybe (XOnlyPubKey, (Profile, DateTime))
tagToProfile datetime (PTag (ValidXOnlyPubKey xo) _ name) = Just (xo,  ( Profile (fromMaybe "" name) Nothing Nothing Nothing, datetime))
tagToProfile _ _ = Nothing

addEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r `sameRelay` r') rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'
    sortByDate a b = compare (created_at $ fst b) (created_at $ fst a)

sendPost :: TChan Request -> FutrModel -> Text -> IO ()
sendPost request model post = do
  now <- getCurrentTime
  let (Keys kp xo _ _) = model ^. selectedKeys
  let unsigned = textNote (strip post) xo now;
  atomically $ writeTChan request $ SendEvent $ signEvent unsigned kp xo
