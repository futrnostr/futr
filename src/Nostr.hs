{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Haskoin.Crypto (Mnemonic, Passphrase)
import Nostr.Event (Event, Rumor, UnsignedEvent)
import Nostr.Event qualified as NE
import Nostr.Keys (KeyPair, PubKeyXO, SecKey)
import Nostr.Keys qualified as Keys
import Nostr.Profile (Profile)
import Nostr.Profile qualified as NP

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


-- | Dispatch type for Nostr effect.
type instance DispatchOf Nostr = Dynamic


-- | Effectful type for Nostr.
type NostrEff es = IOE :> es


createSecKey :: Nostr :> es => Eff es SecKey
createSecKey = send CreateSecKey

createKeyPair :: Nostr :> es => Eff es KeyPair
createKeyPair = send CreateKeyPair

createMnemonic :: Nostr :> es => Eff es (Either String Mnemonic)
createMnemonic = send CreateMnemonic

mnemonicToKeyPair :: Nostr :> es => Mnemonic -> Passphrase -> Eff es (Either String KeyPair)
mnemonicToKeyPair mnemonic passphrase = send $ MnemonicToKeyPair mnemonic passphrase

signEvent :: Nostr :> es => UnsignedEvent -> KeyPair -> Eff es (Maybe Event)
signEvent event kp = send $ SignEvent event kp

createSeal :: Nostr :> es => Rumor -> KeyPair -> PubKeyXO -> Eff es (Maybe Event)
createSeal rumor kp pk = send $ CreateSeal rumor kp pk

createGiftWrap :: Nostr :> es => Event -> PubKeyXO -> Eff es (Maybe (Event, KeyPair))
createGiftWrap event pk = send $ CreateGiftWrap event pk

unwrapGiftWrap :: Nostr :> es => Event -> KeyPair -> Eff es (Maybe Event)
unwrapGiftWrap event kp = send $ UnwrapGiftWrap event kp

unwrapSeal :: Nostr :> es => Event -> KeyPair -> Eff es (Maybe Rumor)
unwrapSeal event kp = send $ UnwrapSeal event kp

verifyNip05 :: Nostr :> es => Profile -> PubKeyXO -> Eff es Bool
verifyNip05 profile pk = send $ VerifyNip05 profile pk


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
