{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nostr.Encryption.Internal where

import Control.Monad (unless, when)
import Crypto.Cipher.ChaCha (generate, initialize)
import Crypto.Hash (SHA256(..))
import Crypto.KDF.HKDF (PRK, expand, extractSkip)
import Crypto.MAC.HMAC (HMAC, hmac)
import qualified Crypto.Secp256k1 as S
import Data.Bits (shiftL, xor)
import Data.ByteString (ByteString)
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get (getWord16be, runGet)
import Data.Binary.Put (runPut, putWord16be)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Nostr.Keys (PubKeyXO(..), exportPubKeyXO)

minPlaintextSize :: Int
minPlaintextSize = 0x0001

maxPlaintextSize :: Int
maxPlaintextSize = 0xffff

{-|
  Derives cryptographic keys for message encryption and authentication using a shared conversation key
  and a nonce, based on the HKDF (HMAC-based Key Derivation Function)

  Takes:
  * `conversationKey` - A ByteString representing the shared secret
  * `nonce` - A ByteString used to ensure distinct keys for each message

  Returns:
  A tuple containing:
  * `chachaKey` (32 bytes) - For ChaCha20 encryption.
  * `chachaNonce` (12 bytes) - Nonce for ChaCha20 encryption.
  * `hmacKey` (remaining bytes) - For message authentication using HMAC.

  Example:

  @
  let (chachaKey, chachaNonce, hmacKey) = getMessageKeys conversationKey nonce
  @
-}
getMessageKeys :: ByteString -> ByteString -> (ByteString, ByteString, ByteString)
getMessageKeys conversationKey nonce = do
  let prk = extractSkip conversationKey :: PRK SHA256
      expandedKeys = hkdfExpand prk nonce
      chachaKey = BS.take 32 expandedKeys
      chachaNonce = BS.take 12 (BS.drop 32 expandedKeys)
      hmacKey = BS.drop 44 expandedKeys
  (chachaKey, chachaNonce, hmacKey)

-- | Calculates the padded length based on the given length.
calcPaddedLen :: Int -> Either String Int
calcPaddedLen len
    | len < 1 = Left errMsgSize
    | len <= 32 = Right 32
    | otherwise = Right $ chunk * ((len - 1) `div` chunk + 1)
  where
    bitShift = ceiling (logBase 2 (fromIntegral (len - 1)) :: Double)
    nextPower = 1 `shiftL` bitShift
    chunk = if nextPower <= 256 then 32 else nextPower `div` 8
    errMsgSize = "invalid plaintext size: must be between 1 and 65535 bytes"

encryptChaCha20 :: ByteString -> ByteString -> ByteString -> ByteString
encryptChaCha20 key nonce padded =
    let state = initialize 20 key nonce
        (keystream, _) = generate state (BS.length padded)
        ciphertext = BS.pack $ zipWith xor (BS.unpack padded) (BS.unpack keystream)
    in ciphertext

decryptChaCha20 :: ByteString -> ByteString -> ByteString -> ByteString
decryptChaCha20 key nonce ciphertext =
    let state = initialize 20 key nonce
        (keystream, _) = generate state (BS.length ciphertext)
        keystreamList = BS.unpack keystream
        ciphertextList = BS.unpack ciphertext
        plaintextList = zipWith xor ciphertextList keystreamList
        plaintext = BS.pack plaintextList
    in plaintext

calculateHmac :: ByteString -> ByteString -> ByteString -> ByteString
calculateHmac key message aad =
  let combined = BS.concat [aad, message]
      hmacResult = hmac key combined :: HMAC SHA256
  in BA.convert hmacResult

-- | Pads the plaintext to match the required length with padding bytes.
padPlaintext :: Text -> Either String ByteString
padPlaintext plaintext =
  let unpadded = encodeUtf8 plaintext
      unpaddedLen = BS.length unpadded
  in if unpaddedLen > maxPlaintextSize || unpaddedLen < minPlaintextSize
     then Left "Invalid plaintext size: must be between 1 and 65535 bytes"
     else do
       paddedLen <- calcPaddedLen unpaddedLen
       if paddedLen >= unpaddedLen
       then Right $ BS.concat [writeU16BE (fromIntegral unpaddedLen), unpadded, BS.replicate (paddedLen - unpaddedLen) 0]
       else Left "Calculated padded length is less than the unpadded length"

unpadPlaintext :: ByteString -> Either String Text
unpadPlaintext padded =
  if isValidPadding padded
    then Right $ decodeUtf8 unpadded
    else Left "invalid padding"
  where
    unpaddedLen = fromIntegral $ runGet getWord16be (BSL.fromStrict $ BS.take 2 padded)
    unpadded = BS.drop 2 $ BS.take (2 + unpaddedLen) padded
    isValidPadding p = case calcPaddedLen unpaddedLen of
      Left _ -> False
      Right pd ->
        let withinBounds = unpaddedLen >= minPlaintextSize && unpaddedLen <= maxPlaintextSize
            correctLength = BS.length unpadded == unpaddedLen
            validPaddingLength = BS.length p == 2 + pd
        in withinBounds && correctLength && validPaddingLength

decodePayload :: Text -> Either String (ByteString, ByteString, ByteString)
decodePayload payloadText = do
    let payload = encodeUtf8 payloadText
    let plen = BS.length payload
    unless (plen >= 132 && plen <= 87472) $ Left $ "Invalid payload length: " ++ show plen

    decoded <- case B64.decode payload of
        Left err -> Left $ "Invalid base64: " ++ err
        Right d -> Right d

    let dlen = BS.length decoded
    unless (dlen >= 99 && dlen <= 65603) $ Left $ "Invalid data length: " ++ show dlen

    let vers = BS.index decoded 0
    when (vers /= 2) $ Left $ "Unknown encryption version: " ++ show vers

    let nonce = BS.take 32 (BS.drop 1 decoded)
        rest = BS.drop 33 decoded
        mac = BS.take 32 (BS.drop (BS.length rest - 32) rest)
        ciphertext = BS.take (BS.length rest - 32) rest

    Right (nonce, ciphertext, mac)

hkdfExpand :: ByteArrayAccess info => PRK SHA256 -> info -> ByteString
hkdfExpand prk info = expand prk info 76 -- 76 bytes to cover all keys

convertToFullPubKey :: PubKeyXO -> Maybe S.PubKeyXY
convertToFullPubKey pk = S.importPubKeyXY $ BS.cons 0x02 (exportPubKeyXO pk)

-- | Converts a length to a 2-byte prefix in big-endian order.
writeU16BE :: Int -> ByteString
writeU16BE len = BSL.toStrict $ runPut $ putWord16be (fromIntegral len)
