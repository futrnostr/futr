{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Presentation.KeyMgmt where

import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), eitherDecode)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, isPrefixOf, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
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
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Nostr.Bech32
import Nostr.Keys
import Nostr.Types hiding (displayName, picture)
import System.FilePath (takeFileName, (</>))
import Text.Read (readMaybe)


-- | Account.
data Account = Account
  { nsec :: SecKey,
    npub :: PubKeyXO,
    displayName :: Maybe Text,
    picture :: Maybe Text,
    relays :: [Relay]
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
initialState :: KeyMgmtState
initialState =
  KeyMgmtState
    { accountMap = Map.empty,
      accountPool = Nothing,
      seedphrase = "",
      npubView = "",
      nsecView = "",
      errorMsg = ""
    }


-- | Key Management Effect.
type KeyMgmtEff es = ( State KeyMgmtState :> es
                     , FileSystem :> es
                     , IOE :> es
                     , EffectfulQML :> es )

-- | Key Management Effects.
data KeyMgmt :: Effect where
  ImportSecretKey :: ObjRef () -> Text -> KeyMgmt m Bool
  ImportSeedphrase :: ObjRef () -> Text -> Text -> KeyMgmt m Bool
  GenerateSeedphrase :: ObjRef () -> KeyMgmt m ()
  RemoveAccount :: ObjRef () -> Text -> KeyMgmt m ()

type instance DispatchOf KeyMgmt = Dynamic


makeEffect ''KeyMgmt


-- | Key Management UI Effect.
type KeyMgmgtUIEff es = (KeyMgmt :> es, State KeyMgmtState :> es, IOE :> es, EffectfulQML :> es, FileSystem :> es)

-- | Key Management Effect for creating QML UI.
data KeyMgmtUI :: Effect where
  CreateUI :: SignalKey (IO ()) -> KeyMgmtUI m (ObjRef ())


-- | Dispatch for Key Management UI Effect.
type instance DispatchOf KeyMgmtUI = Dynamic


makeEffect ''KeyMgmtUI


-- | Handler for the logging effect to stdout.
runKeyMgmt :: KeyMgmtEff es => Eff (KeyMgmt : es) a -> Eff es a
runKeyMgmt = interpret $ \_ -> \case
  ImportSecretKey obj input -> do
    mkp <- tryImportSecretKeyAndPersist input
    case mkp of
      Just kp -> do
        let (ai, ad) = accountFromKeyPair kp
        modify $ \st -> st {accountMap = Map.insert ai ad (accountMap st)}
        fireSignal obj
        return True
      Nothing -> do
        modify $ \st -> st {errorMsg = "Error: Importing secret key failed"}
        fireSignal obj
        return False

  ImportSeedphrase obj input pwd -> do
    mkp <- liftIO $ mnemonicToKeyPair input pwd
    case mkp of
      Right kp -> do
        let secKey = keyPairToSecKey kp
        tryImportSecretKeyAndPersist (secKeyToBech32 secKey) >>= \mkp' ->
          case mkp' of
            Just _ -> do
              let (ai, ad) = accountFromKeyPair kp
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
    mnemonicResult <- liftIO createMnemonic
    case mnemonicResult of
      Left err -> modify $ \st -> st {errorMsg = "Error: " <> pack err}
      Right m' -> do
        keyPairResult <- liftIO $ mnemonicToKeyPair m' ""
        case keyPairResult of
          Left err' -> modify $ \st -> st {errorMsg = "Error: " <> pack err'}
          Right mkp' -> do
            let secKey = keyPairToSecKey mkp'
            maybeKeyPair <- tryImportSecretKeyAndPersist (secKeyToBech32 secKey)
            case maybeKeyPair of
              Just kp -> do
                let (ai, ad) = accountFromKeyPair kp
                modify $ \st ->
                  st
                    { accountMap = Map.insert ai ad (accountMap st),
                      seedphrase = m',
                      nsecView = secKeyToBech32 $ keyPairToSecKey kp,
                      npubView = pubKeyXOToBech32 $ keyPairToPubKeyXO kp
                    }
              Nothing -> modify $ \st -> st {errorMsg = "Error: Unknown error generating new keys"}
    fireSignal obj

  RemoveAccount obj input -> do
    modify $ \st -> st {accountMap = Map.delete (AccountId input) (accountMap st)}
    fireSignal obj
    dir <- getXdgDirectory XdgData $ "futrnostr/" ++ (unpack input)
    directoryExists <- doesDirectoryExist dir
    if directoryExists
      then removeDirectoryRecursive dir
      else return ()


-- | Run the Key Management UI effect.
runKeyMgmtUI :: KeyMgmgtUIEff es => Eff (KeyMgmtUI : es) a -> Eff es a
runKeyMgmtUI action = interpret handleKeyMgmtUI action
  where
    handleKeyMgmtUI :: KeyMgmgtUIEff es => EffectHandler KeyMgmtUI es
    handleKeyMgmtUI _ = \case
      CreateUI changeKey -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        runE loadAccounts

        let prop n f = defPropertySigRO' n changeKey ( \obj -> runE $ do
              st <- get
              let res = maybe "" f $ Map.lookup (fromObjRef obj) (accountMap st)
              return res)

        let mprop n f = defPropertySigRO' n changeKey ( \obj -> runE $ do
              st <- get
              let res = case Map.lookup (fromObjRef obj) (accountMap st) of
                    Just acc -> f acc
                    Nothing -> Nothing
              return res)

        accountClass <-
          newClass
            [ prop "nsec" (secKeyToBech32 . nsec),
              prop "npub" (pubKeyXOToBech32 . npub),
              mprop "displayName" displayName,
              mprop "picture" picture
            ]

        accountPool' <- newFactoryPool (newObject accountClass)

        runE $ modify $ \st -> st {accountPool = Just accountPool'}

        contextClass <-
          newClass
            [ defPropertySigRO' "accounts" changeKey $ \_ -> do
                st <- runE get
                mapM (getPoolObject accountPool') $ Map.keys (accountMap st),
              defMethod' "removeAccount" $ \obj input -> runE $ removeAccount obj input,
              defPropertySigRO' "seedphrase" changeKey $ \_ -> do
                st <- runE get
                return $ seedphrase st,
              defPropertySigRO' "nsec" changeKey $ \_ -> do
                st <- runE get
                return $ nsecView st,
              defPropertySigRO' "npub" changeKey $ \_ -> do
                st <- runE get
                return $ npubView st,
              defPropertySigRW'
                "errorMsg"
                changeKey
                ( \_ -> do
                    st <- runE get
                    return $ errorMsg st
                )
                ( \obj newErrorMsg -> runE $ do
                    modify $ \st -> st {errorMsg = newErrorMsg}
                    fireSignal obj
                    return ()
                ),
              defMethod' "importSecretKey" $ \obj (input :: Text) -> runE $ importSecretKey obj input,
              defMethod' "importSeedphrase" $ \obj input pwd -> runE $ importSeedphrase obj input pwd,
              defMethod' "generateSeedphrase" $ \obj -> runE $ generateSeedphrase obj
            ]

        newObject contextClass ()


-- | Load all accounts from the Nostr data directory.
loadAccounts :: (FileSystem :> es, State KeyMgmtState :> es) => Eff es ()
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
loadAccount :: (FileSystem :> es) => FilePath -> FilePath -> Eff es (Maybe Account)
loadAccount storageDir npubDir = do
  let dirPath = storageDir </> npubDir
  nsecContent <- readFileMaybe (dirPath </> "nsec")
  relayList <- readJSONFile (dirPath </> "relays.json")
  profile <- readJSONFile (dirPath </> "profile.json")

  return $ do
    nsecKey <- bech32ToSecKey . strip =<< nsecContent
    pubKeyXO <- bech32ToPubKeyXO (pack npubDir)

    Just
      Account
        { nsec = nsecKey,
          npub = pubKeyXO,
          relays = fromMaybe defaultRelays relayList,
          displayName = profile >>= \(Profile _ d _ _ _ _) -> d,
          picture = profile >>= \(Profile _ _ _ p _ _) -> p
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


-- | Create an AccountId and Account from a KeyPair.
accountFromKeyPair :: KeyPair -> (AccountId, Account)
accountFromKeyPair kp = (AccountId newNpub, account)
  where
    newNpub = pubKeyXOToBech32 $ keyPairToPubKeyXO kp
    account =
      Account
        { nsec = keyPairToSecKey kp,
          npub = keyPairToPubKeyXO kp,
          relays = defaultRelays,
          displayName = Nothing,
          picture = Nothing
        }
