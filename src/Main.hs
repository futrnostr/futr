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
import Presentation.Welcome
import Types

data AppModel = AppModel
    { keyPair :: Maybe KeyPair
    , currentScreen :: AppScreen
    , welcomeModel :: MVar WelcomeModel
    , keyMgmtModel :: MVar KeyMgmtModel
    } deriving (Typeable)

createContext :: MVar AppModel -> SignalKey (IO ()) -> IO (ObjRef ())
createContext modelVar changeKey = do
    appModel <- readMVar modelVar

    let getKeyPair :: IO (Maybe KeyPair)
        getKeyPair = do
            appModel' <- readMVar modelVar
            return (keyPair appModel')

        setKeyPair :: KeyPair -> IO ()
        setKeyPair kp = modifyMVar_ modelVar $ \m -> return m { keyPair = Just kp }

        setCurrentScreen :: AppScreen -> IO ()
        setCurrentScreen screen = do
            modifyMVar_ modelVar $ \m -> return m { currentScreen = screen }
            --fireSignal changeKey obj -- @todo fix me

    welcomeObj <- createWelcomeCtx (welcomeModel appModel) changeKey getKeyPair setKeyPair setCurrentScreen
    keyMgmtObj <- createKeyMgmtCtx (keyMgmtModel appModel) changeKey setKeyPair setCurrentScreen

    rootClass <- newClass [
        defPropertyConst' "ctxWelcome" (\_ -> return welcomeObj),
        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertySigRW' "currentScreen" changeKey
            (\_ -> fmap (pack . show . currentScreen) (readMVar modelVar))
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        modifyMVar_ modelVar $ \model -> return model { currentScreen = s }
                        fireSignal changeKey obj
                    Nothing -> return ())
        ]

    rootObj <- newObject rootClass ()
    return rootObj


main :: IO ()
main = do
    accounts <- listAccounts

    welcomeM <- newMVar $ WelcomeModel "" ""
    keyMgmtM <- newMVar $ KeyMgmtModel { accountMap = accounts }

    let appModel = AppModel
            { keyPair = Nothing
            , currentScreen = Types.KeyMgmt
            , keyMgmtModel = keyMgmtM
            , welcomeModel = welcomeM
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
