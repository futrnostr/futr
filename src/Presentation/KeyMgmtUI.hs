{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Presentation.KeyMgmtUI where

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Text (Text, unpack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret, send)
import Effectful.FileSystem (FileSystem, XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory)
import Effectful.State.Static.Shared (State, get, modify, put)
import Graphics.QML hiding (fireSignal, runEngineLoop)
import System.FilePath ((</>))

import Downloader (Downloader)
import QtQuick
import KeyMgmt
import Logging
import Nostr
import Nostr.Bech32
import Nostr.InboxModel
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.ProfileManager (ProfileManager)
import Nostr.Publisher
import Nostr.Util
import Store.Lmdb (LmdbState, LmdbStore, initializeLmdbState)
import Types (AppState(..), RelayPool(..), initialRelayPool)


-- | Key Management UI Effect.
type KeyMgmgtUIEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , State LmdbState :> es
  , State QtQuickState :> es
  , ProfileManager :> es
  , LmdbStore :> es
  , Downloader :> es
  , Util :> es
  , Nostr :> es
  , InboxModel :> es
  , Publisher :> es
  , KeyMgmt :> es
  , Concurrent :> es
  , State KeyMgmtState :> es
  , IOE :> es
  , QtQuick :> es
  , FileSystem :> es
  , Logging :> es )

-- | Key Management Effect for creating QML UI.
data KeyMgmtUI :: Effect where
  CreateUI :: SignalKey (IO ()) -> KeyMgmtUI m (ObjRef ())


-- | Dispatch for Key Management UI Effect.
type instance DispatchOf KeyMgmtUI = Dynamic


createUI :: KeyMgmtUI :> es => SignalKey (IO ()) -> Eff es (ObjRef ())
createUI changeKey = send $ CreateUI changeKey


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
            [ prop "nsec" (secKeyToBech32 . accountSecKey),
              prop "npub" (pubKeyXOToBech32 . accountPubKeyXO),
              mprop "displayName" accountDisplayName,
              mprop "picture" accountPicture,
              defMethod' "getProfilePicture" $ \obj pictureUrl -> runE $ do
                let accountId = fromObjRef obj :: AccountId
                st <- get
                let pk = case Map.lookup accountId (accountMap st) of
                          Just account -> accountPubKeyXO account
                          Nothing -> error "Account not found"
                case pictureUrl of
                        Nothing -> pure $ Just $ "https://robohash.org/" <> pubKeyXOToBech32 pk <> ".png?size=50x50"
                        Just "" -> pure $ Just $ "https://robohash.org/" <> pubKeyXOToBech32 pk <> ".png?size=50x50"
                        Just url -> pure $ Just url
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
              defMethod' "createAccount" $ \obj -> runE $ do
                mkp <- generateSeedphrase obj
                case mkp of
                  Just kp -> do
                    modify @AppState $ \s -> s { keyPair = Just kp }
                    modify @RelayPool $ const initialRelayPool

                    -- Create the directory for the account
                    let pk = keyPairToPubKeyXO kp
                    baseDir <- getXdgDirectory XdgData ("futrnostr" </> unpack (pubKeyXOToBech32 pk))
                    createDirectoryIfMissing True baseDir
                    let lmdDir = baseDir </> "db"
                    createDirectoryIfMissing True lmdDir
                    -- Initialize the LMDB database
                    lmdbState <- liftIO $ initializeLmdbState lmdDir
                    put @LmdbState lmdbState

                    void $ async $ startInboxModel
                    return True

                  Nothing -> do
                    logError "Failed to get keypair after generating seedphrase"
                    return False

            ]

        newObject contextClass ()
