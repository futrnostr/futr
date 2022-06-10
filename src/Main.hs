{-# LANGUAGE OverloadedStrings    #-}

module Main where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM (atomically)
import Data.Aeson
import Data.DateTime
import Data.Default
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
    AppInit ->
      [ Task loadKeysFromDisk
      , Producer createProfileCacheDir
      , Producer $ initRelays env
      ]
    GoHome ->
      [ Model $ model & currentView .~ HomeView ]
    GoKeyManagement ->
      [ Model $ model
          & currentView .~ KeyManagementView
          & keyMgmtModel . KeyManagement.keyList .~ model ^. keys
      ]
    AppTypes.GoSetup ->
      [ Model $ model & currentView .~ SetupView ]
    RelaysInitialized rs ->
      [ Model $ model & relays .~ rs ]
    -- keys
    KeyPairsLoaded ks ->
      [ Model $ model
        & keys .~ ks
        & selectedKeys .~ mk
        & currentView .~ HomeView
      ]
      where
        mk = mainKeys ks
        (Keys _ xo _ _) = mk
    NoKeysFound ->
      [ Model $ model & currentView .~ SetupView ]
    ErrorReadingKeysFile ->
      [ Model $ model & errorMsg .~ (Just $ pack "Could not read keys file.\nCheck the file permissions. Maybe the file was corrupted.") ]
    NewKeysCreated ks metadataContent ->
      [ Model $ model
          & keys .~ ks : dk
          & myMetadataContent .~ Just metadataContent
          & selectedKeys .~ ks
          & AppTypes.backupKeysModel . BackupKeys.backupKeys .~ ks
          & currentView .~ BackupKeysView
          & homeModel . Home.profileImage .~ fromMaybe "" picture
      , Task $ saveKeyPairs $ ks : dk
      ]
      where
        dk = disableKeys $ model ^. keys
        MetadataContent _ _ _ picture = metadataContent
    KeysBackupDone ->
      [ Model $ model
          & currentView .~ HomeView
      ]
    KeysUpdated keysList ->
      [ Model $ model
          & keys .~ keysList
          & selectedKeys .~
            if null keysList then initialKeys else head $ filter (\(Keys _ _ active _) -> active == True) keysList
      , Task $ saveKeyPairs keysList
      , if null keysList then Model $ model & currentView .~ SetupView else Monomer.Event NoOp
      ]
    -- relays
    ConnectRelay relay ->
      [ Producer $ connectRelay env relay ]
    DisconnectRelay r ->
      [ Task $ disconnectRelay env r ]
    RelayConnected r ->
      [ Model $ model & relays .~ r : (removeRelayFromList (model ^. relays) r) ]
    RelayDisconnected r ->
      [ Model $ model & relays .~ r : (removeRelayFromList (model ^. relays) r) ]
    -- edit profile
    EditProfile ->
      [ Model $ model
          & currentView .~ EditProfileView
          & editProfileModel . EditProfile.nameInput .~ name
          & editProfileModel . EditProfile.displayNameInput .~ fromMaybe "" displayName
          & editProfileModel . EditProfile.aboutInput .~ fromMaybe "" about
          & editProfileModel . EditProfile.pictureInput .~ fromMaybe "" picture
      ]
      where
        MetadataContent name displayName about picture = fromMaybe def (model ^. myMetadataContent)
    ProfileUpdated metadataContent ->
      [ Model $ model
          & currentView .~ HomeView
          & myMetadataContent .~ Just metadataContent
          & homeModel . Home.profileImage .~ fromMaybe "" picture
          & keys .~ ks' : dk
          & selectedKeys .~ ks'
          & AppTypes.backupKeysModel . BackupKeys.backupKeys .~ ks'
      , Task $ saveKeyPairs $ ks' : dk
      ]
      where
        MetadataContent name displayName about picture = metadataContent
        ks = model ^. selectedKeys
        (Keys pk xo _ _) = ks
        ks' = Keys pk xo True (Just name)
        dk = disableKeys $ filter (\(Keys pk' _ _ _) -> pk' /= pk) $ model ^. keys

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

initRelays :: AppEnv -> (AppEvent -> IO ()) -> IO ()
initRelays env sendMsg = do
  (RelayPool relays _) <- readMVar $ env ^. relayPool
  mapM_ (\relay -> sendMsg $ ConnectRelay relay) relays
  sendMsg $ RelaysInitialized relays

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env relay sendMsg =
  connect (env ^. channel) (env ^. relayPool) sendMsg RelayConnected RelayDisconnected relay

disconnectRelay :: AppEnv -> Relay -> IO AppEvent
disconnectRelay env r = if not $ connected r then return NoOp else do
  atomically $ writeTChan (env ^. channel) $ Disconnect r
  return NoOp

mainKeys :: [Keys] -> Keys
mainKeys ks = head $ filter (\(Keys _ _ xo _) -> xo == True) ks

saveKeyPairs :: [Keys] -> IO AppEvent
saveKeyPairs ks = do
  LazyBytes.writeFile "keys.ft" $ encode ks
  putStrLn "KeyPairs saved to disk"
  return NoOp

createProfileCacheDir :: (AppEvent -> IO ()) -> IO ()
createProfileCacheDir _ = do
  dirExists <- doesDirectoryExist "profiles"
  if dirExists
    then return ()
    else createDirectory "profiles"
