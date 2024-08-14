{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Main where

import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Data.Text (pack, unpack)
import Graphics.QML
import Nostr.Keys
    ( keyPairToPubKeyXO
    , keyPairToSecKey
    , pubKeyXOToBech32
    , secKeyToBech32
    )
import Paths_futr (getDataFileName)
import System.Environment (setEnv)
import Text.Read (readMaybe)

import Types
import Presentation.WelcomeScreen

createContext :: ModelVar -> SignalKey (IO ()) -> IO (ObjRef ())
createContext modelVar changeKey = do
    welcomeScreenObj <- createWelcomeScreenCtx modelVar changeKey

    rootClass <- newClass [
        defPropertyConst' "ctxWelcomeScreen" (\_ -> return welcomeScreenObj),

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
    modelVar <- newMVar $ AppModel { keyPair = Nothing, currentScreen = WelcomeScreen, seedphrase = "", errorMsg = "" }
    changeKey <- newSignalKey :: IO (SignalKey (IO ()))
    ctx <- createContext modelVar changeKey

    path <- getDataFileName "qml/main.qml"
    importPath <- getDataFileName "qml"
    importPath' <- getDataFileName "qml/content"
    importPath'' <- getDataFileName "qml/imports"
    qtQuickControls <- getDataFileName "qml/qtquickcontrols2.conf"

    setEnv "QT_QUICK_CONTROLS_CONF" qtQuickControls
    setEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1"
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"
    setEnv "QT_ENABLE_HIGHDPI_SCALING" "1"

    runEngineLoop defaultEngineConfig 
        { initialDocument = fileDocument path
        , contextObject = Just $ anyObjRef ctx
        , importPaths = [importPath, importPath', importPath'']
        }
