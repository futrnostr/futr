{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Data.Typeable (Typeable)
import Graphics.QML
import System.Environment (setEnv)
import Text.Read (readMaybe)

import Nostr.Keys (KeyPair, secKeyToKeyPair)
import Presentation.KeyMgmt
import Types

data AppModel = AppModel
    { keyPair :: Maybe KeyPair
    , currentScreen :: AppScreen
    , keyMgmtModel :: MVar KeyMgmtModel
    } deriving (Typeable)

createContext :: MVar AppModel -> SignalKey (IO ()) -> IO (ObjRef ())
createContext modelVar changeKey = do
    let getKeyPair' :: IO (Maybe KeyPair)
        getKeyPair' = do
            appModel' <- readMVar modelVar
            return (keyPair appModel')

        setKeyPair' :: KeyPair -> IO ()
        setKeyPair' kp = modifyMVar_ modelVar $ \m -> return m { keyPair = Just kp }

    appModel <- readMVar modelVar
    keyMgmtObj <- createKeyMgmtCtx (keyMgmtModel appModel) changeKey getKeyPair' setKeyPair'

    rootClass <- newClass [
        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertySigRW' "currentScreen" changeKey
            (\_ -> fmap (pack . show . currentScreen) (readMVar modelVar))
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        modifyMVar_ modelVar $ \model -> return model { currentScreen = s }
                        fireSignal changeKey obj
                    Nothing -> return ()),

        defMethod' "login" $ \this input -> do
            appModel' <- readMVar modelVar
            keyMgmtModel' <- readMVar $ keyMgmtModel appModel'
            case Map.lookup (AccountId input) (accountMap keyMgmtModel') of
                Just a -> do
                    modifyMVar_ modelVar $ \m -> return m { keyPair = Just $ secKeyToKeyPair $ nsec a, currentScreen = Home }
                    fireSignal changeKey this
                Nothing ->
                    return ()
        ]

    rootObj <- newObject rootClass ()
    return rootObj


main :: IO ()
main = do
    accounts <- listAccounts
    keyMgmtM <- newMVar $ KeyMgmtModel accounts "" ""

    let appModel = AppModel
            { keyPair = Nothing
            , currentScreen = Types.KeyMgmt
            , keyMgmtModel = keyMgmtM
            }

    modelVar <- newMVar appModel
    changeKey <- newSignalKey :: IO (SignalKey (IO ()))
    ctx <- createContext modelVar changeKey

    let path = "qrc:/qml/main.qml"
    let importPath = "qrc:/qml"
    let importPath' = "qrc:/qml/content"
    let importPath'' = "qrc:/qml/imports"
    let qtQuickControls = "resources/qml/qtquickcontrols2.conf" -- @todo move out of the way

    setEnv "QT_QUICK_CONTROLS_CONF" qtQuickControls
    setEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1"
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"
    setEnv "QT_ENABLE_HIGHDPI_SCALING" "1"

    runEngineLoop defaultEngineConfig
        { initialDocument = fileDocument path
        , contextObject = Just $ anyObjRef ctx
        , importPaths = [importPath, importPath', importPath'']
        }
