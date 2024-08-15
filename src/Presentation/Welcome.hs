{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Presentation.Welcome where

import Control.Concurrent (modifyMVar_, readMVar)
import Data.Text (Text, isPrefixOf, pack, unpack)
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


createWelcomeCtx :: ModelVar -> SignalKey (IO ()) -> IO (ObjRef ())
createWelcomeCtx modelVar changeKey = do
    let handleError :: ObjRef() -> String -> IO ()
        handleError obj err = do
            modifyMVar_ modelVar $ \m -> return m { errorMsg = pack err }
            fireSignal changeKey obj

    contextClass <- newClass [
        defPropertySigRO' "seedphrase" changeKey $ \_ -> fmap seedphrase (readMVar modelVar),

        defPropertySigRW' "errorMsg" changeKey
            (\_ -> fmap errorMsg (readMVar modelVar))
            (\obj newErrorMsg -> handleError obj $ unpack newErrorMsg),

        defPropertySigRO' "nsec" changeKey $ \_ ->
            fmap (maybe (pack "") (secKeyToBech32 . keyPairToSecKey) . keyPair) (readMVar modelVar),

        defPropertySigRO' "npub" changeKey $ \_ ->
            fmap (maybe (pack "") (pubKeyXOToBech32 . keyPairToPubKeyXO) . keyPair) (readMVar modelVar),

        defMethod' "importSecretKey" $ \this (input :: Text) -> do
            mkp <- importSecretKey input
            case mkp of
                Just _ -> do
                    modifyMVar_ modelVar $ \m -> return m { keyPair = mkp, currentScreen = Home }
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
                                modifyMVar_ modelVar $ \m -> return m { keyPair = mkp', currentScreen = Home }
                                fireSignal changeKey this
                            Nothing -> handleError this "Unknown error"
                Left err -> handleError this err,

        defMethod' "generateSeedphrase" $ \this -> do
            createMnemonic >>= either (handleError this) (\m' -> do
                mnemonicToKeyPair m' "" >>= either (handleError this) (\mkp' -> do
                    let secKey = keyPairToSecKey mkp'
                    importSecretKey (secKeyToBech32 secKey) >>= \mkp ->
                        case mkp of
                            Just k -> do
                                modifyMVar_ modelVar $ \m -> return m { seedphrase = m', keyPair = Just k }
                                fireSignal changeKey this
                            Nothing -> handleError this "Unknown error generating new keys"
                    )
                )

        ]

    newObject contextClass ()
