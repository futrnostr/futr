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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Nostr.Keys ( 
    -- * Types
      KeyPair
    , PubKeyXO
    , SecKey
    , Signature

    -- * generation
    , createKeyPair
    , createSecKey
    , createMnemonic
        
    -- * Parsing and Serialization
    , importSecKey
    , exportSecKey
    , importPubKeyXO
    , exportPubKeyXO
    , exportSignature
    , secKeyToBech32
    , pubKeyXOToBech32
    , bech32ToPubKeyXO
    , bech32ToSecKey
    
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

import "libsecp256k1" Crypto.Secp256k1
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
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

-- | Generate a secret key
createSecKey :: IO SecKey
createSecKey = do
    bs <- secKeyGen
    maybe (error "Invalid secret key") return $ importSecKey bs

-- | Generate a key pair
createKeyPair :: IO KeyPair
createKeyPair = keyPairCreate <$> createSecKey

-- | Bech32 encoding for SecKey
secKeyToBech32 :: SecKey -> T.Text
secKeyToBech32 secKey = toBech32 "nsec" (exportSecKey secKey)

-- | Bech32 encoding for PubKeyXO
pubKeyXOToBech32 :: PubKeyXO -> T.Text
pubKeyXOToBech32 pubKeyXO = toBech32 "npub" (exportPubKeyXO pubKeyXO)

-- | Bech32 decoding to SecKey
bech32ToSecKey :: T.Text -> Maybe SecKey
bech32ToSecKey txt = fromBech32 "nsec" txt >>= importSecKey

-- | Bech32 decoding to PubKeyXO
bech32ToPubKeyXO :: T.Text -> Maybe PubKeyXO
bech32ToPubKeyXO txt = fromBech32 "npub" txt >>= importPubKeyXO

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
                    sk' <- maybe (Left "Error, failed to import sec key") Right (importSecKey bs)
                    return $ keyPairCreate sk'

-- | Get the sec key from a key pair
keyPairToSecKey :: KeyPair -> SecKey
keyPairToSecKey = keyPairSecKey

-- | Get the key pair from a sec key
secKeyToKeyPair :: SecKey -> KeyPair
secKeyToKeyPair = keyPairCreate

-- | Get the pub key XO from a key pair
keyPairToPubKeyXO :: KeyPair -> PubKeyXO
keyPairToPubKeyXO kp = p
    where
        (p, _) = keyPairPubKeyXO kp

-- | Derive pub key XO from a secret key
derivePublicKeyXO :: SecKey -> PubKeyXO
derivePublicKeyXO sk = p
    where
        (p, _) = xyToXO $ derivePubKey sk

-- | Exports a signature to hex format
exportSignature :: Signature -> T.Text
exportSignature sig = T.pack hexStr
    where
        hexStr = strip $ show sig

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
