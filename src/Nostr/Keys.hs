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
    , secKeyToBech32
    , pubKeyXOToBech32
    , bech32ToPubKeyXO
    , bech32ToSecKey
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
import Codec.Binary.Bech32 qualified as Bech32
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

-- | Bech32 encoding for SecKey
secKeyToBech32 :: SecKey -> T.Text
secKeyToBech32 secKey = toBech32 "nsec" (S.exportSecKey $ getSecKey secKey)

-- | Bech32 encoding for PubKeyXO
pubKeyXOToBech32 :: PubKeyXO -> T.Text
pubKeyXOToBech32 pubKeyXO = toBech32 "npub" (S.exportPubKeyXO $ getPubKeyXO pubKeyXO)

-- | Bech32 decoding to SecKey
bech32ToSecKey :: T.Text -> Maybe SecKey
bech32ToSecKey txt = fmap SecKey $ fromBech32 "nsec" txt >>= S.importSecKey

-- | Bech32 decoding to PubKeyXO
bech32ToPubKeyXO :: T.Text -> Maybe PubKeyXO
bech32ToPubKeyXO txt = fmap PubKeyXO $ fromBech32 "npub" txt >>= S.importPubKeyXO

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

-- | Convert from ByteString to bech32
toBech32 :: T.Text -> BS.ByteString -> T.Text
toBech32 hrpText bs =
    case Bech32.humanReadablePartFromText hrpText of
        Left err -> error $ "Invalid HRP: " ++ show err
        Right hrp ->
            case Bech32.encode hrp (Bech32.dataPartFromBytes bs) of
                Left err -> error $ "Bech32 encoding failed: " ++ show err
                Right txt -> txt

-- | Convert from bech32 to ByteString
fromBech32 :: T.Text -> T.Text -> Maybe BS.ByteString
fromBech32 hrpText txt =
    case Bech32.humanReadablePartFromText hrpText of
        Left _ -> Nothing
        Right hrp ->
            case Bech32.decode txt of
                Left _ -> Nothing
                Right (hrp', dp) ->
                    if hrp == hrp'
                    then Bech32.dataPartToBytes dp
                    else Nothing

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
