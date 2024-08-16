{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Presentation.Welcome where

import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Typeable (Typeable)
import qualified Data.Text.IO as TIO
import Graphics.QML
import Nostr.Keys
    ( bech32ToSecKey
    , SecKey
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
import System.Directory (XdgDirectory(XdgData), createDirectoryIfMissing, getXdgDirectory)
import Text.Read (readMaybe)

import Types

data WelcomeModel = WelcomeModel
    { seedphrase :: Text
    , errorMsg :: Text
    } deriving (Typeable)

importSecretKey :: Text -> IO (Maybe KeyPair)
importSecretKey input = do
    let skMaybe = if "nsec" `isPrefixOf` input
                  then bech32ToSecKey input
                  else readMaybe (unpack input) :: Maybe SecKey
    case skMaybe of
        Just sk -> do
            storageDir <- getXdgDirectory XdgData $ "futrnostr/" ++ (unpack $ pubKeyXOToBech32 pk)
            _ <- createDirectoryIfMissing True storageDir
            _ <- TIO.writeFile (storageDir ++ "/nsec") content
            return $ Just kp
            where
                pk = derivePublicKeyXO sk
                content = secKeyToBech32 sk
                kp = secKeyToKeyPair sk
        Nothing ->
            return Nothing


createWelcomeCtx
    :: MVar WelcomeModel 
    -> SignalKey (IO ())
    -> IO (Maybe KeyPair)
    -> (KeyPair -> IO ())
    -> (AppScreen -> IO ())
    -> IO (ObjRef ())
createWelcomeCtx modelVar changeKey getKeyPair setKeyPair go = do
    let handleError :: ObjRef() -> String -> IO ()
        handleError obj err = do
            modifyMVar_ modelVar $ \m -> return m { errorMsg = pack err }
            fireSignal changeKey obj

    contextClass <- newClass [
        defPropertySigRO' "seedphrase" changeKey $ \_ -> fmap seedphrase (readMVar modelVar),

        defPropertySigRW' "errorMsg" changeKey
            (\_ -> fmap errorMsg (readMVar modelVar))
            (\obj newErrorMsg -> handleError obj $ unpack newErrorMsg),

        defPropertySigRO' "nsec" changeKey $ \_ -> do
            mkp <- getKeyPair
            case mkp of
                Just kp -> return $ secKeyToBech32 (keyPairToSecKey kp)
                Nothing -> return $ pack "",

        defPropertySigRO' "npub" changeKey $ \_ -> do
            mkp <- getKeyPair
            case mkp of
                Just kp -> return $ pubKeyXOToBech32 (keyPairToPubKeyXO kp)
                Nothing -> return $ pack "",

        defMethod' "importSecretKey" $ \this (input :: Text) -> do
            mkp <- importSecretKey input
            case mkp of
                Just kp -> do
                    setKeyPair kp
                    go Home
                    fireSignal changeKey this
                Nothing -> handleError this "Error: Importing secret key failed",

        defMethod' "importSeedphrase" $ \this input pwd -> do
            mkp <- mnemonicToKeyPair input pwd
            case mkp of
                Right kp -> do
                    let secKey = keyPairToSecKey kp
                    importSecretKey (secKeyToBech32 secKey) >>= \mkp' ->
                        case mkp' of
                            Just _ -> do
                                setKeyPair kp
                                go Home
                                fireSignal changeKey this
                            Nothing -> handleError this "Unknown error"
                Left err -> handleError this err,

        defMethod' "generateSeedphrase" $ \this -> do
            createMnemonic >>= either (handleError this) (\m' -> do
                mnemonicToKeyPair m' "" >>= either (handleError this) (\mkp' -> do
                    let secKey = keyPairToSecKey mkp'
                    importSecretKey (secKeyToBech32 secKey) >>= \mkp ->
                        case mkp of
                            Just kp -> do
                                modifyMVar_ modelVar $ \m -> return m { seedphrase = m' }
                                setKeyPair kp
                                fireSignal changeKey this
                            Nothing -> handleError this "Unknown error generating new keys"
                    )
                )

        ]

    newObject contextClass ()
