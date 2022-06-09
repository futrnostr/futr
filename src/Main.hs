{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad.STM                    (atomically)
import           Data.Aeson
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.DateTime
import           Data.Default
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe
import           Data.Text                            (pack)
import           Monomer
import           Monomer.Widgets.Single
import           System.Directory                     (doesFileExist)

import           AppTypes
import           Helpers
import           Nostr.Keys
import           Nostr.Relay
import           Nostr.RelayConnection
import           Nostr.RelayPool
import           Nostr.Request  as Request
import           UI
import           UIHelpers
import           Widgets.BackupKeys as BackupKeys

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
      , Producer $ initRelays env
      ]
    RelaysInitialized rs ->
      [ Model $ model & relays .~ rs ]
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
      [ Model $ model & errorMsg .~ (Just $ pack "Could not read keys file.\nCheck the file permissions. Maybe the file was corrupted.") ]
    ConnectRelay relay ->
      [ Producer $ connectRelay env relay ]
    DisconnectRelay r ->
      [ Task $ disconnectRelay env r ]
    RelayConnected r ->
      [ Model $ model & relays .~ r : (removeRelayFromList (model ^. relays) r) ]
    RelayDisconnected r ->
      [ Model $ model & relays .~ r : (removeRelayFromList (model ^. relays) r) ]
    NewKeysCreated ks ->
      [ Model $ model
          & keys .~ ks : dk
          & selectedKeys .~ Just ks
          & backupKeysModel . BackupKeys.backupKeys .~ Just ks
          & currentView .~ BackupKeysView
      , Task $ saveKeyPairs $ ks : dk
      ]
      where
        dk = disableKeys $ model ^. keys
    KeysBackupDone ->
      [ Model $ model
          & currentView .~ HomeView
          & backupKeysModel . BackupKeys.backupKeys .~ Nothing
      ]

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
