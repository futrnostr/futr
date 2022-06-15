{-# LANGUAGE OverloadedStrings    #-}

module Main where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM (atomically)
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe
import Data.Text (pack)
import Monomer
import Monomer.Widgets.Single
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist)

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map

import AppTypes
import Helpers
import Nostr.Event
import Nostr.Keys
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayConnection
import Nostr.RelayPool
import Nostr.Request  as Request
import UI
import UIHelpers
import Widgets.BackupKeys as BackupKeys
import Widgets.EditProfile as EditProfile
import Widgets.KeyManagement as KeyManagement
import Widgets.Home as Home

import qualified Widgets.RelayManagement as RelayManagement

main :: IO ()
main = do
  channel <- atomically newBroadcastTChan
  relays <- loadRelaysFromDisk
  poolMVar <- newMVar $ RelayPool relays Map.empty
  startApp def (handleEvent $ AppEnv channel poolMVar) (UI.buildUI channel poolMVar) config
  where
    config =
      [ appWindowTitle "futr - nostr client"
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
    AppInit ->
      [ Task loadKeysFromDisk
      , Producer createProfileCacheDir
      , Producer $ initRelays env
      ]
    RelaysInitialized rs ->
      [ Model $ model & relays .~ rs ]
    -- go to
    GoHome ->
      [ Model $ model & currentView .~ HomeView ]
    GoKeyManagement ->
      [ Model $ model
          & currentView .~ KeyManagementView
          & keyMgmtModel . KeyManagement.keyList .~ model ^. keys
          & keyMgmtModel . KeyManagement.kmProfiles .~ model ^. profiles
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
        & selectedKeys .~ mk
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
          & profiles .~ Map.insert xo (profile, datetime) (model ^. profiles)
          & selectedKeys .~ ks
          & AppTypes.backupKeysModel . BackupKeys.backupKeys .~ ks
          & currentView .~ BackupKeysView
          & homeModel . Home.profileImage .~ fromMaybe "" picture
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
          & selectedKeys .~
            if null keysList then initialKeys else head $ filter (\(Keys _ _ active _) -> active == True) keysList
      , Task $ saveKeyPairs (model ^. keys) keysList
      , if null keysList then Model $ model & currentView .~ SetupView else Monomer.Event NoOp
      ]
    -- relays
    ConnectRelay relay ->
      [ Producer $ connectRelay env relay ]
    AppTypes.RelaysUpdated rs ->
      [ Model $ model
          & relays .~ rs
          & relayMgmtModel . RelayManagement.rmRelays .~ rs
      ]
    -- edit profile
    EditProfile ->
      [ Model $ model
          & currentView .~ EditProfileView
          & editProfileModel . EditProfile.nameInput .~ name
          & editProfileModel . EditProfile.displayNameInput .~ fromMaybe "" displayName
          & editProfileModel . EditProfile.aboutInput .~ fromMaybe "" about
          & editProfileModel . EditProfile.pictureInput .~ fromMaybe "" picture
          & editProfileModel . EditProfile.epProfiles .~ model ^. profiles
          & editProfileModel . EditProfile.currentImage .~ fromMaybe "" pic
      ]
      where
        Keys _ xo _ _ = model ^. selectedKeys
        Profile name displayName about picture = fst $ fromJust $ Map.lookup xo (model ^. profiles)
        pic = do
          ((Profile _ _ _ picture), _) <- Map.lookup xo (model ^. profiles)
          p <- picture
          return p
    ProfileUpdated ks profile datetime ->
      [ Model $ model
          & homeModel . Home.profileImage .~ (
            if ks `sameKeys` (model ^. selectedKeys)
              then fromMaybe "" picture
              else model ^. homeModel.profileImage
            )
          & keys .~ ks' : newKeyList
          & selectedKeys .~ (
            if ks `sameKeys` (model ^. selectedKeys)
              then ks'
              else (model ^. selectedKeys)
            )
          & AppTypes.backupKeysModel . BackupKeys.backupKeys .~ (
            if ks `sameKeys` (model ^. selectedKeys)
              then ks'
              else (model ^. selectedKeys)
            )
          & profiles .~
            case Map.lookup xo (model ^. profiles) of
              Nothing ->
                Map.insert xo (profile, datetime) (model ^. profiles)
              Just (profile', datetime') ->
                if datetime > datetime'
                  then Map.insert xo (profile', datetime) (model ^. profiles)
                  else model ^. profiles
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

initRelays :: AppEnv -> (AppEvent -> IO ()) -> IO ()
initRelays env sendMsg = do
  (RelayPool relays _) <- readMVar $ env ^. relayPool
  mapM_ (\relay -> sendMsg $ ConnectRelay relay) relays
  sendMsg $ RelaysInitialized relays

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env relay sendMsg =
  connect (env ^. channel) (env ^. relayPool) sendMsg RelaysUpdated relay

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

createProfileCacheDir :: (AppEvent -> IO ()) -> IO ()
createProfileCacheDir _ = do
  dirExists <- doesDirectoryExist "profiles"
  if dirExists
    then return ()
    else createDirectory "profiles"
