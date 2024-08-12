{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, takeMVar, putMVar, readMVar)
import Data.Text (Text, isPrefixOf, pack, unpack)
import qualified Data.Text.IO as TIO
import Data.Typeable (Typeable)
import Graphics.QML
import Nostr.Keys
    ( bech32ToSecKey
    , SecKey
    , PubKeyXO
    , KeyPair
    , createMnemonic
    , derivePublicKeyXO
    , keyPairToPubKeyXO
    , keyPairToSecKey
    , mnemonicToKeyPair
    , pubKeyXOToBech32
    , secKeyToBech32
    , secKeyToKeyPair
    )
import Paths_futr (getDataFileName)
import System.Environment (setEnv)
import System.Directory (XdgDirectory(XdgData), createDirectoryIfMissing, getXdgDirectory)
import Text.Read (readMaybe)

data AppScreen
    = WelcomeScreen
    | SelectAccountScreen
    | RelayScreen
    | CreateAccountScreen
    | HomeScreen
    deriving (Eq, Read, Show)

data AppModel = AppModel
    { keyPair :: Maybe KeyPair
    , seedphrase :: Text
    , availableKeys :: [PubKeyXO]
    , currentScreen :: AppScreen
    , errorMsg :: Text
    } deriving (Typeable)

type ModelVar = MVar AppModel

createContext :: ModelVar -> SignalKey (IO ()) -> IO (ObjRef ())
createContext modelVar changeKey = do
    rootClass <- newClass [            
        defPropertySigRW' "seedphrase" changeKey 
            (\_ -> do
                model <- readMVar modelVar
                return $ seedphrase model)
            (\obj newSeedphrase -> do
                modifyMVar_ modelVar $ \model -> return model { seedphrase = newSeedphrase }
                fireSignal changeKey obj),

        defPropertySigRO' "nsec" changeKey $ \_ -> 
            fmap (maybe (pack "") (secKeyToBech32 . keyPairToSecKey) . keyPair) (readMVar modelVar),

        defPropertySigRO' "npub" changeKey $ \_ -> 
            fmap (maybe (pack "") (pubKeyXOToBech32 . keyPairToPubKeyXO) . keyPair) (readMVar modelVar),

        defPropertySigRO' "availableKeys" changeKey $ \_ -> do
            model <- readMVar modelVar
            return $ map pubKeyXOToBech32 (availableKeys model),

        defPropertySigRW' "currentScreen" changeKey
            (\_ -> do
                model <- readMVar modelVar
                return $ pack $ show $ currentScreen model)
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        modifyMVar_ modelVar $ \model -> return model { currentScreen = s }
                        fireSignal changeKey obj
                    Nothing -> return ()),

        defPropertySigRW' "errorMsg" changeKey 
            (\_ -> do
                model <- readMVar modelVar
                return $ errorMsg model)
            (\obj newErrorMsg -> do
                modifyMVar_ modelVar $ \model -> return model { errorMsg = newErrorMsg }
                fireSignal changeKey obj),

        defMethod' "importSecretKey" $ \this (input :: Text) -> do
            mkp <- importSecretKey input
            case mkp of
                Just _ -> do
                    model <- takeMVar modelVar
                    putMVar modelVar model { keyPair = mkp, currentScreen = HomeScreen }
                    fireSignal changeKey this
                Nothing -> do
                    model <- takeMVar modelVar
                    putMVar modelVar model { errorMsg = "Error: Importing secret key failed" }
                    fireSignal changeKey this,                    

        defMethod' "generateSeedphrase" $ \this -> do
            let handleError :: String -> IO ()
                handleError err = do
                    model <- takeMVar modelVar
                    putMVar modelVar model { errorMsg = "Error: " <> pack err }
                    fireSignal changeKey this

            createMnemonic >>= either handleError (\m' -> do
                mnemonicToKeyPair m' "" >>= either handleError (\mkp' -> do
                    let secKey = keyPairToSecKey mkp'
                    importSecretKey (secKeyToBech32 secKey) >>= \mkp ->
                        case mkp of
                            Just _ -> do
                                model <- takeMVar modelVar
                                putMVar modelVar model { seedphrase = m', keyPair = mkp }
                                fireSignal changeKey this
                            Nothing -> handleError "Unknown error generating new keys"
                    )
                )

        ]

    newObject rootClass ()



importSecretKey :: Text -> IO (Maybe KeyPair)
importSecretKey input = do
    let skMaybe = if "nsec" `isPrefixOf` input
                  then bech32ToSecKey input
                  else readMaybe (unpack input) :: Maybe SecKey
    case skMaybe of
        Just sk -> do
            storageDir <- getXdgDirectory XdgData $ "futrnostr/" ++ (unpack $ pubKeyXOToBech32 pk)
            _ <- createDirectoryIfMissing True storageDir
            _ <- TIO.writeFile (storageDir ++ "/keys.json") content
            return $ Just kp
            where
                pk = derivePublicKeyXO sk
                content = "[" <> secKeyToBech32 sk <> ", " <> pubKeyXOToBech32 pk <> "]"
                kp = secKeyToKeyPair sk
        Nothing ->
            return Nothing


main :: IO ()
main = do
    modelVar <- newMVar $ AppModel { keyPair = Nothing, availableKeys = [], currentScreen = WelcomeScreen, seedphrase = "", errorMsg = "" }
    changeKey <- newSignalKey :: IO (SignalKey (IO ()))
    ctx <- createContext modelVar changeKey

    path <- getDataFileName "resources/qml/main.qml"
    importPath <- getDataFileName "resources/qml"
    importPath' <- getDataFileName "resources/qml/content"
    importPath'' <- getDataFileName "resources/qml/imports"

    let qtImportPath = "/home/sasa/Qt/qtdesignstudio-2.3.1-community/qt5_design_studio_reduced_version/qml"

    setEnv "QML2_IMPORT_PATH" qtImportPath
    setEnv "QT_QUICK_CONTROLS_STYLE" "Material"
    -- QT_QUICK_CONTROLS_CONF: "qtquickcontrols2.conf"
    setEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1"
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"
    setEnv "QT_ENABLE_HIGHDPI_SCALING" "1"

    -- @todo this must be setup when installing Qt
    -- export LD_LIBRARY_PATH=/home/sasa/Qt/qtdesignstudio-2.3.1-community/qt5_design_studio_reduced_version/lib:$LD_LIBRARY_PATH


    runEngineLoop defaultEngineConfig 
        { initialDocument = fileDocument path
        , contextObject = Just $ anyObjRef ctx
        , importPaths = [importPath, importPath', importPath'', qtImportPath]
        --, pluginPaths = ["/home/sasa/Qt/qtdesignstudio-2.3.1-community/qt5_design_studio_reduced_version/plugins"]
        }
