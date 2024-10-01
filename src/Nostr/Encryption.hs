-- | Module: Nostr.Encryption
--
-- This module provides functionalities for encrypting and decrypting messages
-- within the Nostr network. It includes the following features:
--
-- * Encryption and Decryption:
--     - Encrypts and decrypts messages using ChaCha20 encryption and HMAC-SHA256
--
-- * Key Derivation:
--     - Derives a conversation key from a secret key and an extended public key
--     - Utilizes HKDF (HMAC-based Key Derivation Function) for key expansion
--
-- The functions provided in this module ensure secure message encryption and decryption,
-- following the NIP-44 standard for key derivation.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nostr.Encryption (decrypt, encrypt, getConversationKey) where

import Crypto.Hash (SHA256(..))
import Crypto.KDF.HKDF (PRK, extract)
import qualified Crypto.Secp256k1 as S
import Data.ByteString (ByteString)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Nostr.Encryption.Internal
import Nostr.Keys (PubKeyXO(..), SecKey(..))

-- | Encrypts a message
encrypt :: Text -> ByteString -> ByteString -> Either String Text
encrypt plaintext conversationKey nonce = do
    let (chachaKey, chachaNonce, hmacKey) = getMessageKeys conversationKey nonce
    paddedResult <- padPlaintext plaintext
    ciphertext <- Right (encryptChaCha20 chachaKey chachaNonce paddedResult)
    let mac = calculateHmac hmacKey ciphertext nonce
    Right $ decodeUtf8 $ B64.encode $ BS.concat [BS.singleton 2, nonce, ciphertext, BA.convert mac]

-- | Decrypts a message
decrypt :: ByteString -> Text -> Either String Text
decrypt conversationKey payload = do
    (nonce, ciphertext, macFromMsg) <- decodePayload payload

    let (chachaKey, chachaNonce, hmacKey) = getMessageKeys conversationKey nonce
        calculatedMac = calculateHmac hmacKey ciphertext nonce

    if calculatedMac /= macFromMsg
      then Left "HMAC verification failed"
      else do
        let padded = decryptChaCha20 chachaKey chachaNonce ciphertext
        unpadPlaintext padded

-- | Derives the conversation key using SecKey and PubKeyXO
getConversationKey :: SecKey -> PubKeyXO -> Maybe ByteString
getConversationKey secKey pk = do
    pubKeyXY <- convertToFullPubKey pk
    let tweak = fromJust $ S.importTweak $ S.exportSecKey (getSecKey secKey)
    sharedSecret <- S.pubKeyTweakMul pubKeyXY tweak
    let sharedSecret' = BS.drop 1 $ S.exportPubKeyXY True sharedSecret
    let salt = C8.pack "nip44-v2"
    let prk = extract salt sharedSecret' :: PRK SHA256
    return $ BA.convert prk
