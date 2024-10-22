{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import Haskoin.Crypto (Mnemonic, Passphrase)
import Nostr.Event qualified as NE
import Nostr.Keys (KeyPair, PubKeyXO, SecKey)
import Nostr.Keys qualified as Keys
import Nostr.Profile qualified as NP
import Nostr.Types (Event, Profile, Rumor, UnsignedEvent)

data Nostr :: Effect where
  -- Keys related
  CreateSecKey :: Nostr m SecKey
  CreateKeyPair :: Nostr m KeyPair
  CreateMnemonic :: Nostr m (Either String Mnemonic)
  MnemonicToKeyPair :: Mnemonic -> Passphrase -> Nostr m (Either String KeyPair)
  -- Event related
  SignEvent :: UnsignedEvent -> KeyPair -> Nostr m (Maybe Event)
  CreateSeal :: Rumor -> KeyPair -> PubKeyXO -> Nostr m (Maybe Event)
  CreateGiftWrap :: Event -> PubKeyXO -> Nostr m (Maybe (Event, KeyPair))
  UnwrapGiftWrap :: Event -> KeyPair -> Nostr m (Maybe Event)
  UnwrapSeal :: Event -> KeyPair -> Nostr m (Maybe Rumor)

  -- Profile related
  VerifyNip05 :: Profile -> PubKeyXO -> Nostr m Bool


-- | Dispatch type for Futr effect.
type instance DispatchOf Nostr = Dynamic


makeEffect ''Nostr


-- | Effectful type for Nostr.
type NostrEff es = ( IOE :> es )


-- | Run the Nostr effect.
runNostr :: NostrEff es => Eff (Nostr : es) a -> Eff es a
runNostr = interpret $ \_ -> \case
  -- Keys related
  CreateSecKey -> liftIO Keys.createSecKey
  CreateKeyPair -> liftIO Keys.createKeyPair
  CreateMnemonic -> liftIO Keys.createMnemonic
  MnemonicToKeyPair mnemonic passphrase -> liftIO $ Keys.mnemonicToKeyPair mnemonic passphrase
  -- Event related
  SignEvent event keyPair -> liftIO $ NE.signEvent event keyPair
  CreateSeal rumor keyPair pubkeyXO -> liftIO $ NE.createSeal rumor keyPair pubkeyXO
  CreateGiftWrap event pubkeyXO -> liftIO $ NE.createGiftWrap event pubkeyXO
  UnwrapGiftWrap event keyPair -> liftIO $ NE.unwrapGiftWrap event keyPair
  UnwrapSeal event keyPair -> liftIO $ NE.unwrapSeal event keyPair

  -- Profile related
  VerifyNip05 profile pubkey -> liftIO $ NP.verifyNip05 profile pubkey
