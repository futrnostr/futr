{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies      #-}

module Presentation.KeyMgmt where

import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Monad (filterM)
import Data.Aeson (FromJSON(..), eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, isPrefixOf, pack, strip, unpack)
import Data.Typeable (Typeable)
import qualified Data.Text.IO as TIO
import Graphics.QML
import System.Directory (XdgDirectory(XdgData), createDirectoryIfMissing,
                         getXdgDirectory, listDirectory, doesDirectoryExist,
                         doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)

import Nostr.Keys hiding (getKeyPair)
import Nostr.Profile
import Nostr.Relay (RelayInfo, RelayURI, defaultRelays)
--import Types

data Account = Account
    { nsec :: SecKey
    , npub :: PubKeyXO
    , relays :: [(RelayURI, RelayInfo)]
    , displayName :: Text
    , picture :: Text
    } deriving (Eq, Show)

newtype AccountId = AccountId {accountId :: Text} deriving (Eq, Ord, Show, Typeable)

data KeyMgmtModel = KeyMgmtModel
    { accountMap :: Map AccountId Account
    , seedphrase :: Text
    , errorMsg :: Text
    }

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
    relayList <- readJSONFile (dirPath </> "relays.json")
    profile <- readJSONFile (dirPath </> "profile.json")
    
    return $ do
        nsecKey <- bech32ToSecKey . strip =<< nsecContent
        pubKeyXO <- bech32ToPubKeyXO (pack npubDir)

        Just Account
            { nsec = nsecKey
            , npub = pubKeyXO
            , relays = fromMaybe defaultRelays relayList
            , displayName = maybe "" id (profile >>= \(Profile _ d _ _ _) -> d)
            , picture = maybe ("https://robohash.org/" <> pack npubDir <> ".png") id (profile >>= \(Profile _ _ _ p _) -> p)
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

addNewAccount :: MVar KeyMgmtModel -> KeyPair -> IO (Maybe Text)
addNewAccount modelVar kp = do
    let newNpub = pubKeyXOToBech32 $ keyPairToPubKeyXO kp
    let newAccountId = AccountId newNpub
    let newAccount = Account
            { nsec = keyPairToSecKey kp
            , npub = keyPairToPubKeyXO kp
            , relays = defaultRelays
            , displayName = ""
            , picture =  pack "https://robohash.org/" <> newNpub <> pack ".png"
            }
    modifyMVar_ modelVar $ \m ->
        return m { seedphrase = "", errorMsg = "", accountMap = Map.insert newAccountId newAccount (accountMap m) }
    return $ Just newNpub

createKeyMgmtCtx
    :: MVar KeyMgmtModel 
    -> SignalKey (IO ())
    -> IO (Maybe KeyPair)
    -> (KeyPair -> IO ())
    -> IO (ObjRef ())
createKeyMgmtCtx modelVar changeKey getKeyPair setKeyPair = do
    let handleError :: ObjRef() -> String -> IO ()
        handleError obj err = do
            modifyMVar_ modelVar $ \m -> return m { errorMsg = pack err }
            fireSignal changeKey obj

    let prop n f = defPropertySigRO' n changeKey (\obj -> do
            model <- readMVar modelVar
            return $ maybe "" f $ Map.lookup (fromObjRef obj) (accountMap model))

    accountClass <- newClass [
        prop "nsec" (secKeyToBech32 . nsec),
        prop "npub" (pubKeyXOToBech32 . npub),
        prop "displayName" displayName,
        prop "picture" picture
        ]            

    accountPool <- newFactoryPool (newObject accountClass)

    contextClass <- newClass [
        defPropertySigRO' "accounts" changeKey $ \_ -> do
            model <- readMVar modelVar
            mapM (getPoolObject accountPool) $ Map.keys (accountMap model),

        defMethod' "removeAccount" $ \this input -> do
            modifyMVar_ modelVar $ \m -> do
                let updatedMap = Map.delete (AccountId input) (accountMap m)
                return m { accountMap = updatedMap }
            removeAccount input
            fireSignal changeKey this,

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
                    newNpub <- addNewAccount modelVar kp
                    fireSignal changeKey this
                    return newNpub
                Nothing -> do
                    handleError this "Error: Importing secret key failed"
                    return Nothing,

        defMethod' "importSeedphrase" $ \this input pwd -> do
            mkp <- mnemonicToKeyPair input pwd
            case mkp of
                Right kp -> do
                    let secKey = keyPairToSecKey kp
                    importSecretKey (secKeyToBech32 secKey) >>= \mkp' ->
                        case mkp' of
                            Just _ -> do
                                newNpub <- addNewAccount modelVar kp
                                fireSignal changeKey this
                                return newNpub
                            Nothing -> do
                                handleError this "Unknown error"
                                return Nothing
                Left err -> do
                    handleError this err
                    return Nothing,

        defMethod' "generateSeedphrase" $ \this -> do
            createMnemonic >>= either (\err -> handleError this err >> return Nothing) (\m' -> do
                mnemonicToKeyPair m' "" >>= either (\err -> handleError this err >> return Nothing) (\mkp' -> do
                    let secKey = keyPairToSecKey mkp'
                    importSecretKey (secKeyToBech32 secKey) >>= \mkp ->
                        case mkp of
                            Just kp -> do
                                newNpub <- addNewAccount modelVar kp
                                setKeyPair kp
                                modifyMVar_ modelVar $ \m -> return m { seedphrase = m' }
                                fireSignal changeKey this
                                return newNpub
                            Nothing -> do
                                handleError this "Unknown error generating new keys"
                                return Nothing
                    )
                )

        ]

    newObject contextClass ()
  