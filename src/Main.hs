{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Main where

import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Data.Text (pack, unpack)
import Graphics.QML
import System.Environment (setEnv)
import Text.Read (readMaybe)

import Types
import Presentation.Welcome

createContext :: ModelVar -> SignalKey (IO ()) -> IO (ObjRef ())
createContext modelVar changeKey = do
    welcomeObj <- createWelcomeCtx modelVar changeKey

    rootClass <- newClass [
        defPropertyConst' "ctxWelcome" (\_ -> return welcomeObj),

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
    modelVar <- newMVar $ AppModel { keyPair = Nothing, currentScreen = Welcome, seedphrase = "", errorMsg = "" }
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
