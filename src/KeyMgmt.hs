{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module KeyMgmt where

import Control.Monad (filterM, replicateM)
import Data.Aeson (FromJSON (..), eitherDecode, encode)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, isPrefixOf, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem
  ( FileSystem,
    XdgDirectory (XdgData),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
    removeDirectoryRecursive,
  )
import Effectful.FileSystem.IO.ByteString qualified as FIOE (readFile, writeFile)
import Effectful.FileSystem.IO.ByteString.Lazy qualified as BL
import Effectful.State.Static.Shared (State, gets, modify)
import Graphics.QML hiding (fireSignal, runEngineLoop)
import System.Random (randomRIO)

import QtQuick
import Logging
import Nostr
import Nostr.Bech32
import Nostr.Keys ( KeyPair, PubKeyXO, SecKey, derivePublicKeyXO
                  , keyPairToPubKeyXO, keyPairToSecKey, secKeyToKeyPair)
import Nostr.Profile (Profile(..))
import Nostr.Relay (Relay)
import System.FilePath (takeFileName, (</>))
import Text.Read (readMaybe)
import Types (AppState(..))


-- | Account.
data Account = Account
  { accountSecKey :: SecKey,
    accountPubKeyXO :: PubKeyXO,
    accountDisplayName :: Maybe Text,
    accountPicture :: Maybe Text
  }
  deriving (Eq, Show)


-- | Account ID.
newtype AccountId = AccountId {accountId :: Text} deriving (Eq, Ord, Show, Typeable)


-- | Key Management State.
data KeyMgmtState = KeyMgmtState
  { accountMap :: Map AccountId Account,
    accountPool :: Maybe (FactoryPool AccountId),
    seedphrase :: Text,
    npubView :: Text,
    nsecView :: Text,
    errorMsg :: Text
  }


-- | Initial Key Management State.
initialKeyMgmtState :: KeyMgmtState
initialKeyMgmtState =
  KeyMgmtState
    { accountMap = Map.empty,
      accountPool = Nothing,
      seedphrase = "",
      npubView = "",
      nsecView = "",
      errorMsg = ""
    }


-- | Key Management Effects.
data KeyMgmt :: Effect where
  ImportSecretKey :: ObjRef () -> Text -> KeyMgmt m Bool
  ImportSeedphrase :: ObjRef () -> Text -> Text -> KeyMgmt m Bool
  GenerateSeedphrase :: ObjRef () -> KeyMgmt m (Maybe KeyPair)
  RemoveAccount :: ObjRef () -> Text -> KeyMgmt m ()
  UpdateProfile :: AccountId -> Profile -> KeyMgmt m ()

type instance DispatchOf KeyMgmt = Dynamic


-- | Key Management Effect.
type KeyMgmtEff es = ( State KeyMgmtState :> es
                     , State AppState :> es
                     , Nostr :> es
                     , FileSystem :> es
                     , IOE :> es
                     , QtQuick :> es
                     , Logging :> es )


importSecretKey :: KeyMgmt :> es => ObjRef () -> Text -> Eff es Bool
importSecretKey obj input = send $ ImportSecretKey obj input

importSeedphrase :: KeyMgmt :> es => ObjRef () -> Text -> Text -> Eff es Bool
importSeedphrase obj seedphrase' passphrase = send $ ImportSeedphrase obj seedphrase' passphrase

generateSeedphrase :: KeyMgmt :> es => ObjRef () -> Eff es (Maybe KeyPair)
generateSeedphrase obj = send $ GenerateSeedphrase obj

removeAccount :: KeyMgmt :> es => ObjRef () -> Text -> Eff es ()
removeAccount obj input = send $ RemoveAccount obj input

updateProfile :: KeyMgmt :> es => AccountId -> Profile -> Eff es ()
updateProfile accountId' profile = send $ UpdateProfile accountId' profile


-- | Run the Key Management effect.
runKeyMgmt :: KeyMgmtEff es => Eff (KeyMgmt : es) a -> Eff es a
runKeyMgmt = interpret $ \_ -> \case
  ImportSecretKey obj input -> do
    mkp <- tryImportSecretKeyAndPersist input
    case mkp of
      Just kp -> do
        (ai, ad) <- accountFromKeyPair kp
        modify $ \st -> st {accountMap = Map.insert ai ad (accountMap st)}
        fireSignal obj
        return True
      Nothing -> do
        modify $ \st -> st {errorMsg = "Error: Importing secret key failed"}
        fireSignal obj
        return False

  ImportSeedphrase obj input pwd -> do
    mkp <- mnemonicToKeyPair input pwd
    case mkp of
      Right kp -> do
        let secKey = keyPairToSecKey kp
        tryImportSecretKeyAndPersist (secKeyToBech32 secKey) >>= \mkp' ->
          case mkp' of
            Just _ -> do
              (ai, ad) <- accountFromKeyPair kp
              modify $ \st -> st {accountMap = Map.insert ai ad (accountMap st)}
              fireSignal obj
              return True
            Nothing -> do
              modify $ \st -> st {errorMsg = "Error: Seedphrase generation failed"}
              fireSignal obj
              return False
      Left err -> do
        modify $ \st -> st {errorMsg = "Error: " <> pack err}
        fireSignal obj
        return False

  GenerateSeedphrase obj -> do
    mnemonicResult <- createMnemonic
    case mnemonicResult of
      Left err -> do
        modify $ \st -> st {errorMsg = "Error: " <> pack err}
        fireSignal obj
        return Nothing
      Right m' -> do
        keyPairResult <- mnemonicToKeyPair m' ""
        case keyPairResult of
          Left err' -> do
            modify $ \st -> st {errorMsg = "Error: " <> pack err'}
            fireSignal obj
            return Nothing
          Right mkp' -> do
            let secKey = keyPairToSecKey mkp'
            maybeKeyPair <- tryImportSecretKeyAndPersist (secKeyToBech32 secKey)
            case maybeKeyPair of
              Just kp -> do
                (ai, ad) <- accountFromKeyPair kp
                modify $ \st ->
                  st
                    { accountMap = Map.insert ai ad (accountMap st),
                      seedphrase = m',
                      nsecView = secKeyToBech32 $ keyPairToSecKey kp,
                      npubView = pubKeyXOToBech32 $ keyPairToPubKeyXO kp
                    }
                fireSignal obj
                return (Just kp)
              Nothing -> do
                modify $ \st -> st {errorMsg = "Error: Unknown error generating new keys"}
                fireSignal obj
                return Nothing

  RemoveAccount obj input -> do
    modify $ \st -> st {accountMap = Map.delete (AccountId input) (accountMap st)}
    fireSignal obj
    dir <- getXdgDirectory XdgData $ "futrnostr/" ++ (unpack input)
    directoryExists <- doesDirectoryExist dir
    if directoryExists
      then removeDirectoryRecursive dir
      else return ()

  UpdateProfile aid profile -> do
    modify $ \st -> st
      { accountMap = Map.adjust (\acc -> acc
          { accountDisplayName = displayName profile
          , accountPicture = picture profile
          }) aid (accountMap st)
      }
    accounts <- gets accountMap
    case Map.lookup aid accounts of
      Just account -> do
        let npubStr = unpack $ pubKeyXOToBech32 $ accountPubKeyXO account
        dir <- getXdgDirectory XdgData $ "futrnostr/" ++ npubStr
        BL.writeFile (dir </> "profile.json") (encode profile)
      Nothing -> error $ "Account not found: " <> show (accountId aid)


-- | Load all accounts from the Nostr data directory.
loadAccounts :: (FileSystem :> es, State KeyMgmtState :> es, IOE :> es) => Eff es ()
loadAccounts = do
  storageDir <- getXdgDirectory XdgData "futrnostr"
  directoryExists <- doesDirectoryExist storageDir
  if directoryExists
    then do
      contents <- listDirectory storageDir
      npubDirs <- filterM (isNpubDirectory storageDir) contents
      accounts <- mapM (loadAccount storageDir) npubDirs
      let accountPairs = catMaybes $ zipWith (\dir acc -> fmap (\a -> (AccountId $ pack dir, a)) acc) npubDirs accounts
      modify $ \st -> st {accountMap = Map.fromList accountPairs}
    else modify $ \st -> st {accountMap = Map.empty}


-- | Try to import a secret key and persist it.
tryImportSecretKeyAndPersist :: (FileSystem :> es) => Text -> Eff es (Maybe KeyPair)
tryImportSecretKeyAndPersist input = do
  let skMaybe =
        if "nsec" `isPrefixOf` input
          then bech32ToSecKey input
          else readMaybe (unpack input) :: Maybe SecKey
  case skMaybe of
    Just sk -> do
      storageDir <- getXdgDirectory XdgData $ "futrnostr/" ++ (unpack $ pubKeyXOToBech32 pk)
      createDirectoryIfMissing True storageDir
      FIOE.writeFile (storageDir ++ "/nsec") (encodeUtf8 $ secKeyToBech32 sk)
      return $ Just kp
      where
        pk = derivePublicKeyXO sk
        kp = secKeyToKeyPair sk
    Nothing ->
      return Nothing

-- | Check if a directory is a Nostr pubkey.
isNpubDirectory :: (FileSystem :> es) => FilePath -> FilePath -> Eff es Bool
isNpubDirectory storageDir dirName = do
  let fullPath = storageDir </> dirName
  isDir <- doesDirectoryExist fullPath
  let fileName = takeFileName fullPath
  return $ isDir && "npub" `isPrefixOf` pack fileName


-- | Load an account from a Nostr pubkey directory.
loadAccount :: (FileSystem :> es, IOE :> es) => FilePath -> FilePath -> Eff es (Maybe Account)
loadAccount storageDir npubDir = do
  let dirPath = storageDir </> npubDir
  nsecContent <- readFileMaybe (dirPath </> "nsec")
  profile <- readJSONFile (dirPath </> "profile.json")

  return $ do
    nsecKey <- bech32ToSecKey . strip =<< nsecContent
    pubKeyXO <- bech32ToPubKeyXO (pack npubDir)

    Just
      Account
        { accountSecKey = nsecKey,
          accountPubKeyXO = pubKeyXO,
          accountDisplayName = profile >>= \(Profile _ d _ _ _ _) -> d,
          accountPicture = profile >>= \(Profile _ _ _ p _ _) -> p
        }

-- | Read a file and return its contents as a Maybe Text.
readFileMaybe :: (FileSystem :> es) => FilePath -> Eff es (Maybe Text)
readFileMaybe path = do
  exists <- doesFileExist path
  if exists
    then Just <$> decodeUtf8 <$> FIOE.readFile path
    else return Nothing


-- | Read a JSON file and return its contents as a Maybe value of the specified type.
readJSONFile :: (FromJSON a, FileSystem :> es) => FilePath -> Eff es (Maybe a)
readJSONFile path = do
  exists <- doesFileExist path
  if exists
    then eitherDecode <$> BL.readFile path >>= return . either (const Nothing) Just
    else return Nothing


-- | Select random relays from a list
selectRandomRelays :: Int -> [Relay] -> IO [Relay]
selectRandomRelays count relays = do
  indices <- replicateM count $ randomRIO (0, length relays - 1)
  return $ nub $ map (relays !!) indices

-- | Create an AccountId and Account from a KeyPair.
accountFromKeyPair :: (IOE :> es) => KeyPair -> Eff es (AccountId, Account)
accountFromKeyPair kp = do
  --let (allRelays, _) = defaultGeneralRelays
  --selectedRelays <- liftIO $ selectRandomRelays 3 allRelays
  let newNpub = pubKeyXOToBech32 $ keyPairToPubKeyXO kp
  let account =
        Account
          { accountSecKey = keyPairToSecKey kp,
            accountPubKeyXO = keyPairToPubKeyXO kp,
            accountDisplayName = Nothing,
            accountPicture = Nothing
          }
  return (AccountId newNpub, account)
