-- | Module: Nostr.Keys
--
-- This module provides the necessary data types and functions for working with cryptographic keys
-- in the context of Nostr. It includes the following functionalities:
--
-- * Data types:
--     - 'KeyPair': Represents a public/private key pair.
--     - 'PubKeyXO': Represents an extended-only public key.
--     - 'SecKey': Represents a secret key.
--     - 'Signature': Represents a Schnorr signature.
--
-- * Key operations:
--     - Key generation
--     - Mnemonic generation and handling
--     - Key derivation following NIP-06
--     - Bech32 encoding
--     - Schnorr signature creation and verification
--
-- The functions provided in this module ensure secure and efficient cryptographic operations
-- for key management and transaction signing in the Nostr network.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Nostr.Keys (
    -- * Types
      KeyPair(..)
    , PubKeyXO(..)
    , SecKey(..)
    , Signature(..)

    -- * generation
    , createKeyPair
    , createSecKey
    , createMnemonic

    -- * Parsing and Serialization
    , importSecKey
    , exportSecKey
    , importPubKeyXO
    , importSignature
    , exportPubKeyXO
    , exportSignature
    , byteStringToHex

    -- * Conversions
    , derivePublicKeyXO
    , keyPairToSecKey
    , keyPairToPubKeyXO
    , mnemonicToKeyPair
    , secKeyToKeyPair

    -- * Schnorr signatures
    , schnorrSign
    , schnorrVerify
    ) where

import Crypto.Secp256k1 qualified as S
import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as C
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Haskoin.Crypto
    ( Mnemonic
    , Passphrase
    , DerivPath
    , DerivPathI((:/), Deriv, (:|))
    , XPrvKey(XPrvKey)
    , createContext
    , derivePath
    , makeXPrvKey
    , mnemonicToSeed
    , toMnemonic
    )
import System.Entropy (getEntropy)
import System.Random (newStdGen, randoms)

newtype KeyPair = KeyPair { getKeyPair :: S.KeyPair } deriving (Eq)
newtype SecKey = SecKey { getSecKey :: S.SecKey } deriving (Eq, Ord, Read, Show)
newtype PubKeyXO = PubKeyXO { getPubKeyXO :: S.PubKeyXO } deriving (Eq, Generic, Ord, Read, Show)
newtype Signature = Signature { getSignature :: S.SchnorrSignature } deriving (Eq, Generic, Read, Show)

instance FromJSON PubKeyXO where
  parseJSON = withText "PubKeyXO" $ \t -> do
    decoded <- either fail return $ B16.decode (C.pack $ T.unpack t)
    case importPubKeyXO decoded of
      Just pk -> return pk
      Nothing -> fail $ "Invalid PubKeyXO: " ++ T.unpack t

instance ToJSON PubKeyXO where
  toJSON = String . byteStringToHex . exportPubKeyXO
  toEncoding = text . byteStringToHex . exportPubKeyXO

instance FromJSON Signature where
  parseJSON = withText "Signature" $ \t -> do
    decoded <- either fail return $ B16.decode (C.pack $ T.unpack t)
    case S.importSchnorrSignature decoded of
      Just sig -> return $ Signature sig
      Nothing -> fail "Invalid Signature"

instance ToJSON Signature where
  toJSON = String . byteStringToHex . exportSignature
  toEncoding = text . byteStringToHex . exportSignature

-- | Generate a secret key
createSecKey :: IO SecKey
createSecKey = do
    bs <- secKeyGen
    maybe (error "Unknown error upon sec key generation") (return . SecKey) $ S.importSecKey bs

-- | Generate a key pair
createKeyPair :: IO KeyPair
createKeyPair = do
    bs <- secKeyGen
    sk <- maybe (error "Unknown error upon key pair generation") return $ S.importSecKey bs
    return $ KeyPair $ S.keyPairCreate sk

-- | Import byte string and create SecKey
importSecKey :: ByteString -> Maybe SecKey
importSecKey = fmap SecKey . S.importSecKey

-- | Export SecKey to byte string
exportSecKey :: SecKey -> ByteString
exportSecKey = S.exportSecKey . getSecKey

-- | Import byte string and create PubKeyXO
importPubKeyXO :: ByteString -> Maybe PubKeyXO
importPubKeyXO = fmap PubKeyXO . S.importPubKeyXO

-- | Export PubKeyXO to byte string
exportPubKeyXO :: PubKeyXO -> ByteString
exportPubKeyXO = S.exportPubKeyXO . getPubKeyXO

-- | Import byte string and create Signature
importSignature :: ByteString -> Maybe Signature
importSignature = fmap Signature . S.importSchnorrSignature

-- | Export signature to byte string
exportSignature :: Signature -> ByteString
exportSignature = S.exportSchnorrSignature . getSignature

-- | Convert byte string to hex string
byteStringToHex :: ByteString -> T.Text
byteStringToHex = decodeUtf8 . B16.encode

-- | Sign message using Schnorr signature scheme
schnorrSign :: KeyPair -> ByteString -> IO (Maybe Signature)
schnorrSign (KeyPair kp) msg = fmap (fmap Signature) $ S.schnorrSignNondeterministic kp msg

-- | Verify Schnorr signature
schnorrVerify :: PubKeyXO -> ByteString -> Signature -> Bool
schnorrVerify (PubKeyXO pk) msg (Signature sig) = S.schnorrVerify pk msg sig

-- | Generate a new mnemonic seed
createMnemonic :: IO (Either String Mnemonic)
createMnemonic = toMnemonic <$> getEntropy 16

-- | Get the key pair from a mnemonic
mnemonicToKeyPair :: Mnemonic -> Passphrase -> IO (Either String KeyPair)
mnemonicToKeyPair m p
    | not (isValid12WordMnemonic m) = return $ Left "Mnemonic must have exactly 12 words."
    | otherwise = do
        case mnemonicToSeed p m of
            Left err -> return $ Left err
            Right seed -> do
                ctx <- createContext
                let master = makeXPrvKey seed
                let XPrvKey _ _ _ _ sk = derivePath ctx nostrAddr master
                let hexStr = strip $ show sk
                return $ do
                    bs <- hexToByteString hexStr
                    sk' <- maybe (Left "Error, failed to import sec key") Right (S.importSecKey bs)
                    return $ KeyPair $ S.keyPairCreate sk'

-- | Get the sec key from a key pair
keyPairToSecKey :: KeyPair -> SecKey
keyPairToSecKey = SecKey . S.keyPairSecKey . getKeyPair

-- | Get the key pair from a sec key
secKeyToKeyPair :: SecKey -> KeyPair
secKeyToKeyPair = KeyPair . S.keyPairCreate . getSecKey

-- | Get the pub key XO from a key pair
keyPairToPubKeyXO :: KeyPair -> PubKeyXO
keyPairToPubKeyXO kp = PubKeyXO p
    where
        (p, _) = S.keyPairPubKeyXO (getKeyPair kp)

-- | Derive pub key XO from a secret key
derivePublicKeyXO :: SecKey -> PubKeyXO
derivePublicKeyXO sk = PubKeyXO p
    where
        (p, _) = S.xyToXO $ S.derivePubKey (getSecKey sk)

-- Utility functions

-- | Generate a random byte sequence for a secret key
secKeyGen :: IO BS.ByteString
secKeyGen = BS.pack . take 32 . randoms <$> newStdGen

-- | Derivation path for Nostr (NIP-06)
nostrAddr :: DerivPath
nostrAddr = Deriv :| 44 :| 1237 :| 0 :/ 0 :/ 0

-- | Convert hex string to ByteString
hexToByteString :: String -> Either String C.ByteString
hexToByteString str =
  case B16.decode $ C.pack str of
    Left err -> Left $ "Decoding error: " ++ err
    Right bytes -> Right bytes

-- | Strip first and last char of a string
strip :: String -> String
strip [] = []
strip [_] = []
strip xs = tail (init xs)

-- | Check if a mnemonic has exactly 12 words
isValid12WordMnemonic :: Mnemonic -> Bool
isValid12WordMnemonic mnemonic = length (words $ T.unpack mnemonic) == 12
