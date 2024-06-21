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
    , createSecretKey
    , generateMnemonic
        
    -- * Parsing and Serialization
    , importSecKey
    , exportSecKey
    , importPubKeyXO
    , exportPubKeyXO
    , secKeyToBech32
    , pubKeyXOToBech32
    , bech32ToPubKeyXO
    , bech32ToSecKey
    
    -- * Conversions
    , derivePubKey
    , keyPairCreate
    , keyPairSecKey
    , keyPairPubKeyXO
    , xyToXO
    , mnemonicToKeyPair

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
createSecretKey :: IO SecKey
createSecretKey = do
    bs <- secKeyGen
    maybe (error "Invalid secret key") return $ importSecKey bs

-- | Generate a key pair
createKeyPair :: IO KeyPair
createKeyPair = keyPairCreate <$> createSecretKey

-- Bech32 encoding for SecKey
secKeyToBech32 :: SecKey -> T.Text
secKeyToBech32 secKey = toBech32 "nsec" (exportSecKey secKey)

-- Bech32 encoding for PubKeyXO
pubKeyXOToBech32 :: PubKeyXO -> T.Text
pubKeyXOToBech32 pubKeyXO = toBech32 "npub" (exportPubKeyXO pubKeyXO)

-- Bech32 decoding to SecKey
bech32ToSecKey :: T.Text -> Maybe SecKey
bech32ToSecKey txt = fromBech32 "nsec" txt >>= importSecKey

-- Bech32 decoding to PubKeyXO
bech32ToPubKeyXO :: T.Text -> Maybe PubKeyXO
bech32ToPubKeyXO txt = fromBech32 "npub" txt >>= importPubKeyXO

-- Generate a new mnemonic seed
generateMnemonic :: IO (Either String Mnemonic)
generateMnemonic = toMnemonic <$> getEntropy 16

-- 
mnemonicToKeyPair :: Mnemonic -> Passphrase -> IO (Either String KeyPair)
mnemonicToKeyPair m p = do
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

-- Utility functions

-- | Generate a random byte sequence for a secret key
secKeyGen :: IO BS.ByteString
secKeyGen = BS.pack . take 32 . randoms <$> newStdGen

-- Convert from ByteString to bech32
toBech32 :: T.Text -> BS.ByteString -> T.Text
toBech32 hrpText bs =
    case Bech32.humanReadablePartFromText hrpText of
        Left err -> error $ "Invalid HRP: " ++ show err
        Right hrp ->
            case Bech32.encode hrp (Bech32.dataPartFromBytes bs) of
                Left err -> error $ "Bech32 encoding failed: " ++ show err
                Right txt -> txt

-- Convert from bech32 to ByteString
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

-- Derivation path for Nostr (NIP-06)
nostrAddr :: DerivPath
nostrAddr = Deriv :| 44 :| 1237 :| 0 :/ 0 :/ 0

-- Convert hex string to ByteString
hexToByteString :: String -> Either String C.ByteString
hexToByteString str =
  case B16.decode $ C.pack str of
    Left err -> Left $ "Decoding error: " ++ err
    Right bytes -> Right bytes

-- Strip first and last char of a string
strip :: String -> String
strip [] = []
strip [_] = []
strip xs = tail (init xs)
