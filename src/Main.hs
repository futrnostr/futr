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

        defPropertySigRO' "nsec" changeKey $ \_ ->
            fmap (maybe (pack "") (secKeyToBech32 . keyPairToSecKey) . keyPair) (readMVar modelVar),

        defPropertySigRO' "npub" changeKey $ \_ -> 
            fmap (maybe (pack "") (pubKeyXOToBech32 . keyPairToPubKeyXO) . keyPair) (readMVar modelVar),

        defPropertySigRO' "availableKeys" changeKey $ \_ -> do
            model <- readMVar modelVar
            return $ map pubKeyXOToBech32 (availableKeys model),

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
    modelVar <- newMVar $ AppModel { keyPair = Nothing, availableKeys = [], currentScreen = WelcomeScreen, seedphrase = "", errorMsg = "" }
    changeKey <- newSignalKey :: IO (SignalKey (IO ()))
    ctx <- createContext modelVar changeKey

    path <- getDataFileName "resources/qml/main.qml"
    importPath <- getDataFileName "resources/qml"
    importPath' <- getDataFileName "resources/qml/content"
    importPath'' <- getDataFileName "resources/qml/imports"
    qtQuickControls <- getDataFileName "resources/qml/qtquickcontrols2.conf"

    setEnv "QT_QUICK_CONTROLS_CONF" qtQuickControls
    setEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1"
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"
    setEnv "QT_ENABLE_HIGHDPI_SCALING" "1"

    runEngineLoop defaultEngineConfig 
        { initialDocument = fileDocument path
        , contextObject = Just $ anyObjRef ctx
        , importPaths = [importPath, importPath', importPath'']
        }
