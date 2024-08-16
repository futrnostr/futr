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
import Presentation.Accounts
import Presentation.Welcome
import Types

data AppModel = AppModel
    { keyPair :: Maybe KeyPair
    , currentScreen :: AppScreen
    , welcomeModel :: MVar WelcomeModel
    , accountModel :: MVar AccountModel
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
        setCurrentScreen screen = modifyMVar_ modelVar $ \m -> return m { currentScreen = screen }

    welcomeObj <- createWelcomeCtx (welcomeModel appModel) changeKey getKeyPair setKeyPair setCurrentScreen
    accountObj <- createAccountCtx (accountModel appModel) changeKey setKeyPair setCurrentScreen

    rootClass <- newClass [
        defPropertyConst' "ctxWelcome" (\_ -> return welcomeObj),
        defPropertyConst' "ctxAccounts" (\_ -> return accountObj),

        defPropertySigRW' "currentScreen" changeKey
            (\_ -> fmap (pack . show . currentScreen) (readMVar modelVar))
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        modifyMVar_ modelVar $ \model -> return model { currentScreen = s }
                        fireSignal changeKey obj
                    Nothing -> return ())
        ]

    newObject rootClass ()


main :: IO ()
main = do
    accounts <- listAccounts

    welcomeM <- newMVar $ WelcomeModel "" ""
    accountM <- newMVar $ AccountModel { accountMap = accounts }

    let appModel = case Map.size accounts of
            0 -> AppModel
                { keyPair = Nothing
                , currentScreen = Welcome
                , accountModel = accountM
                , welcomeModel = welcomeM
                }
            1 -> AppModel
                { keyPair = Just $ secKeyToKeyPair $ nsec $ snd $ head $ Map.toList accounts
                , currentScreen = Home
                , accountModel = accountM
                , welcomeModel = welcomeM
                }
            _ -> AppModel
                { keyPair = Nothing
                , currentScreen = Types.Account
                , accountModel = accountM
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
