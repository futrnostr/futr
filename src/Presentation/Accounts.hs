{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies      #-}

module Presentation.Accounts where

import Control.Concurrent (MVar, modifyMVar_, readMVar, withMVar)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, isPrefixOf, pack, strip, unpack)
import Data.Typeable (Typeable)
import qualified Data.Text.IO as TIO
import Graphics.QML
import System.Directory (XdgDirectory(XdgData), getXdgDirectory, listDirectory, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>), takeFileName)

import Nostr.Keys (KeyPair, PubKeyXO, SecKey, bech32ToPubKeyXO, bech32ToSecKey, pubKeyXOToBech32, secKeyToBech32)
import Nostr.Relay (RelayInfo, RelayURI, defaultRelays)
import Types (AppScreen)

data Account = Account
    { nsec :: SecKey
    , npub :: PubKeyXO
    , relays :: [(RelayURI, RelayInfo)]
    , displayName :: Text
    , picture :: Text
    } deriving (Eq, Show)

newtype AccountId = AccountId {accountId :: Text} deriving (Eq, Ord, Show, Typeable)

data AccountModel = AccountModel { accountMap :: Map AccountId Account }

listAccounts :: IO (Map AccountId Account)
listAccounts = do
    storageDir <- getXdgDirectory XdgData "futrnostr"
    directoryExists <- doesDirectoryExist storageDir
    if directoryExists
        then do
            contents <- listDirectory storageDir
            npubDirs <- filterM (isNpubDirectory storageDir) contents
            accounts <- mapM (loadAccount storageDir) npubDirs
            let accountPairs = catMaybes $ zipWith (\dir acc -> fmap (\a -> (AccountId $ pack dir, a)) acc) npubDirs accounts
            return $ Map.fromList accountPairs
        else return Map.empty

removeAccount:: Text -> IO ()
removeAccount a = do
    dir <- getXdgDirectory XdgData $ "futrnostr/" ++ (unpack a)
    directoryExists <- doesDirectoryExist dir
    if directoryExists
        then removeDirectoryRecursive dir
        else return ()

isNpubDirectory :: FilePath -> FilePath -> IO Bool
isNpubDirectory storageDir name = do
    let fullPath = storageDir </> name
    isDir <- doesDirectoryExist fullPath
    let fileName = takeFileName fullPath
    return $ isDir && "npub" `isPrefixOf` pack fileName

loadAccount :: FilePath -> FilePath -> IO (Maybe Account)
loadAccount storageDir npubDir = do
    let dirPath = storageDir </> npubDir
    nsecContent <- readFileMaybe (dirPath </> "nsec")
    
    case (bech32ToSecKey . strip =<< nsecContent) of
        Nothing -> 
            return Nothing
        Just nsecKey -> do
            relayList <- readJSONFile (dirPath </> "relays.json")
            let relayData = fromMaybe defaultRelays relayList
            
            profile <- readJSONFile (dirPath </> "profile.json")
            let (someName, somePicture) = case profile of
                    Just [name, pic] -> (name, pic)
                    _ -> ("", "")
            
            let maybePubKeyXO = bech32ToPubKeyXO (pack npubDir)
            case maybePubKeyXO of
                Nothing -> return Nothing
                Just pubKeyXO -> return $ Just Account
                    { nsec = nsecKey
                    , npub = pubKeyXO
                    , relays = relayData
                    , displayName = someName
                    , picture = somePicture
                    }

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe path = do
    exists <- doesFileExist path
    if exists
        then Just <$> TIO.readFile path
        else return Nothing

readJSONFile :: FromJSON a => FilePath -> IO (Maybe a)
readJSONFile path = do
    exists <- doesFileExist path
    if exists
        then eitherDecode <$> BL.readFile path >>= return . either (const Nothing) Just
        else return Nothing

createAccountCtx
    :: MVar AccountModel 
    -> SignalKey (IO ())
    -> (KeyPair -> IO ())
    -> (AppScreen -> IO ())
    -> IO (ObjRef ())
createAccountCtx modelVar changeKey setKeyPair go = do
    let p n f = defPropertySigRO' n changeKey (\obj -> do
            model <- readMVar modelVar
            return $ maybe "" f $ Map.lookup (fromObjRef obj) (accountMap model))

    accountClass <- newClass [
        p "nsec" (secKeyToBech32 . nsec),
        p "npub" (pubKeyXOToBech32 . npub),
        p "displayName" displayName,
        p "picture" picture
        ]

    accountPool <- newFactoryPool (newObject accountClass)

    contextClass <- newClass [
        defPropertySigRO' "accounts" changeKey $ \_ -> do
            model <- readMVar modelVar
            mapM (getPoolObject accountPool) $ Map.keys (accountMap model),

        defMethod' "removeAccount" $ \this (input :: Text) -> do
            modifyMVar_ modelVar $ \m -> do
                let updatedMap = Map.delete (AccountId input) (accountMap m)
                return m { accountMap = updatedMap }
            removeAccount input
            fireSignal changeKey this
        ]

    newObject contextClass ()
  