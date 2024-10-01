{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Nostr.EncryptionTest where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson hiding (Key)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Random (newStdGen, randoms)
import Test.Tasty
import Test.Tasty.HUnit

import Nostr.Encryption
import Nostr.Encryption.Internal
import Nostr.Keys

-- Define the data structures for different sections of the JSON file

-- For get_conversation_key vectors
data GetConversationKeyVector = GetConversationKeyVector
  { sec1 :: Text
  , pub2 :: Text
  , conversationKey :: Maybe Text
  , note :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GetConversationKeyVector where
  parseJSON = withObject "GetConversationKeyVector" $ \v -> GetConversationKeyVector
    <$> v .: "sec1"
    <*> v .: "pub2"
    <*> v .:? "conversation_key"
    <*> v .:? "note"

data InvalidGetConversationKeyVector = InvalidGetConversationKeyVector
  { invalidSec1 :: Text
  , invalidPub2 :: Text
  } deriving (Generic, Show)

instance FromJSON InvalidGetConversationKeyVector where
  parseJSON = withObject "InvalidGetConversationKeyVector" $ \v -> InvalidGetConversationKeyVector
    <$> v .: "sec1"
    <*> v .: "pub2"

-- For get_message_keys vectors
data MessageKeyVector = MessageKeyVector
  { messageConversationKey :: Text
  , keys :: [Key]
  } deriving (Generic, Show)

instance FromJSON MessageKeyVector where
  parseJSON = withObject "MessageKeyVector" $ \v -> MessageKeyVector
    <$> v .: "conversation_key"
    <*> v .: "keys"

data Key = Key
  { nonce :: Text
  , chachaKey :: Text
  , chachaNonce :: Text
  , hmacKey :: Text
  } deriving (Generic, Show)

instance FromJSON Key where
  parseJSON = withObject "Key" $ \k -> Key
    <$> k .: "nonce"
    <*> k .: "chacha_key"
    <*> k .: "chacha_nonce"
    <*> k .: "hmac_key"

-- For encrypt_decrypt vectors
data EncryptDecryptVector = EncryptDecryptVector
  { encSec1 :: Text
  , encSec2 :: Text
  , encConversationKey :: Text
  , encNonce :: Text
  , plaintext :: Text
  , payload :: Text
  } deriving (Generic, Show)

instance FromJSON EncryptDecryptVector where
  parseJSON = withObject "EncryptDecryptVector" $ \v -> EncryptDecryptVector
    <$> v .: "sec1"
    <*> v .: "sec2"
    <*> v .: "conversation_key"
    <*> v .: "nonce"
    <*> v .: "plaintext"
    <*> v .: "payload"

-- For invalid encrypt_decrypt vectors
data InvalidEncryptDecryptVector = InvalidEncryptDecryptVector
  { invEncConversationKey :: Text
  , invEncNonce :: Text
  , invPlaintext :: Text
  , invPayload :: Text
  , invNote :: Text
  } deriving (Generic, Show)

instance FromJSON InvalidEncryptDecryptVector where
  parseJSON = withObject "InvalidEncryptDecryptVector" $ \v -> InvalidEncryptDecryptVector
    <$> v .: "conversation_key"
    <*> v .: "nonce"
    <*> v .: "plaintext"
    <*> v .: "payload"
    <*> v .: "note"

-- For long message encryption/decryption
data EncryptDecryptLongMsgVector = EncryptDecryptLongMsgVector
  { longMsgConversationKey :: Text
  , longMsgNonce :: Text
  , pattern :: Text
  , longMsgRepeat :: Int
  , plaintextSha256 :: Text
  , payloadSha256 :: Text
  } deriving (Generic, Show)

instance FromJSON EncryptDecryptLongMsgVector where
  parseJSON = withObject "EncryptDecryptLongMsgVector" $ \v -> EncryptDecryptLongMsgVector
    <$> v .: "conversation_key"
    <*> v .: "nonce"
    <*> v .: "pattern"
    <*> v .: "repeat"
    <*> v .: "plaintext_sha256"
    <*> v .: "payload_sha256"

-- For invalid vectors
data InvalidVectors = InvalidVectors
  { encryptMsgLengths :: [Int]
  , invalidGetConversationKey :: [InvalidGetConversationKeyVector]
  , invalidDecrypt :: [InvalidEncryptDecryptVector]
  } deriving (Generic, Show)

instance FromJSON InvalidVectors where
  parseJSON = withObject "InvalidVectors" $ \v -> InvalidVectors
    <$> v .: "encrypt_msg_lengths"
    <*> v .: "get_conversation_key"
    <*> v .: "decrypt"

-- For valid vectors
data ValidVectors = ValidVectors
  { validGetConversationKey :: [GetConversationKeyVector]
  , validGetMessageKeys :: MessageKeyVector
  , calcPaddedLength :: [(Int, Int)]
  , encryptDecrypt :: [EncryptDecryptVector]
  , encryptDecryptLongMsg :: [EncryptDecryptLongMsgVector]
  } deriving (Generic, Show)

instance FromJSON ValidVectors where
  parseJSON = withObject "ValidVectors" $ \v -> ValidVectors
    <$> v .: "get_conversation_key"
    <*> v .: "get_message_keys"
    <*> v .: "calc_padded_len"
    <*> v .: "encrypt_decrypt"
    <*> v .: "encrypt_decrypt_long_msg"

data V2Wrapper = V2Wrapper
  { valid :: ValidVectors
  , invalid :: InvalidVectors
  } deriving (Generic, Show)

instance FromJSON V2Wrapper where
  parseJSON = withObject "V2Wrapper" $ \v -> V2Wrapper
    <$> v .: "valid"
    <*> v .: "invalid"

-- Combined test vectors
data TestVectors = TestVectors
  { v2 :: V2Wrapper
  } deriving (Generic, Show)

instance FromJSON TestVectors where
  parseJSON = withObject "TestVectors" $ \v -> TestVectors
    <$> v .: "v2"

-- Helper functions
hexToByteString :: Text -> ByteString
hexToByteString str =
  case B16.decode (E.encodeUtf8 str) of
    Left err -> error $ "Decoding error: " ++ err
    Right bytes -> bytes

sha256HexText :: Text -> Text
sha256HexText inputText =
  let inputBytes = E.encodeUtf8 inputText
      digest = hash inputBytes :: Digest SHA256
      hashBytes = BA.convert digest
  in E.decodeUtf8 (B16.encode hashBytes)

random32Bytes :: IO ByteString
random32Bytes = BS.pack . take 32 . randoms <$> newStdGen

testGetConversationKey :: GetConversationKeyVector -> TestTree
testGetConversationKey tv =
  testCase "Get Conversation Key" $ do
    let sk = fromJust $ importSecKey $ hexToByteString (sec1 tv)
    let pk = fromJust $ importPubKeyXO $ hexToByteString (pub2 tv)
    let expectedKey = hexToByteString (fromJust $ conversationKey tv)

    let result = getConversationKey sk pk
    assertEqual "Conversation key should match" (Just expectedKey) result

testGetMessageKeys :: MessageKeyVector -> TestTree
testGetMessageKeys tv =
  testCase "Get Message Keys" $ do
    let conversationKey' = hexToByteString (messageConversationKey tv)
        keyList = keys tv

    forM_ keyList $ \key -> do
      let (chachaKeyRes, chachaNonceRes, hmacKeyRes) = getMessageKeys conversationKey' (hexToByteString $ nonce key)
      assertEqual "Chacha key should match" chachaKeyRes (hexToByteString $ chachaKey key)
      assertEqual "Chacha nonce should match" chachaNonceRes (hexToByteString $ chachaNonce key)
      assertEqual "HMAC key should match" hmacKeyRes (hexToByteString $ hmacKey key)

testCalcPaddedLength :: (Int, Int) -> TestTree
testCalcPaddedLength (len, expected) =
  testCase "Calc Padded Lengths" $ case calcPaddedLen len of
      Left err -> assertFailure $ "Calculation of padding failed: " ++ err
      Right actual -> assertEqual "Length should match" expected actual

testValidEncryptDecrypt :: EncryptDecryptVector -> TestTree
testValidEncryptDecrypt tv =
  testCase "Valid Encrypt / Decrypt" $ do
    let sk1 = fromJust $ importSecKey $ hexToByteString (encSec1 tv)
    let sk2 = fromJust $ importSecKey $ hexToByteString (encSec2 tv)
    let conversationKey' = hexToByteString (encConversationKey tv)
    let nonce' = hexToByteString (encNonce tv)
    let plaintext' = plaintext tv
    let payload' = payload tv

    let pub2' = derivePublicKeyXO sk2
    let key = getConversationKey sk1 pub2'
    assertEqual "Conversation key should match" (Just conversationKey') key

    case encrypt plaintext' (fromJust key) nonce' of
      Left err -> assertFailure $ "Encryption failed with error: " ++ err
      Right ciphertext -> do
        assertEqual "Ciphertext should match" payload' ciphertext

        case decrypt (fromJust key) ciphertext of
            Right decryptedText -> assertEqual "Plaintext should match" plaintext' decryptedText
            Left err -> assertFailure $ "Decryption failed with error: " ++ err

testValidEncryptDecryptLongMsg:: EncryptDecryptLongMsgVector -> TestTree
testValidEncryptDecryptLongMsg tv = do
  testCase "Valid Encrypt / Decrypt Long Msg" $ do
    let conversationKey' = hexToByteString (longMsgConversationKey tv)
    let longMsgNonce' = hexToByteString (longMsgNonce tv)
    let plaintext' = T.replicate (longMsgRepeat tv) (pattern tv)

    assertEqual "Hashed plaintext should match" (sha256HexText plaintext') (plaintextSha256 tv)

    case encrypt plaintext' conversationKey' longMsgNonce' of
      Left err -> assertFailure $ "Encryption failed with error: " ++ err
      Right ciphertext -> do
        assertEqual "should match" (sha256HexText ciphertext) (payloadSha256 tv)

        case decrypt conversationKey' ciphertext of
            Left err -> assertFailure $ "Decryption failed with error: " ++ err
            Right text -> assertEqual "Decrypted text should match" plaintext' text

testInvalidMessageLengths :: Int -> TestTree
testInvalidMessageLengths i =
  testCase "Invalid Message Lengths" $ do
    sec <- random32Bytes
    nonce' <- random32Bytes
    assertBool "Expected error" (isLeft $ encrypt (T.replicate i "a") sec nonce')

testInvalidGetConversationKey :: InvalidGetConversationKeyVector -> TestTree
testInvalidGetConversationKey tv =
  testCase "Invalid Get Conversation Key" $ do
    let sk = importSecKey $ hexToByteString (invalidSec1 tv)
    let pk = importPubKeyXO $ hexToByteString (invalidPub2 tv)

    assertBool "At least one of the values should be Nothing" (isNothing sk || isNothing pk)

testInvalidDecrypt :: InvalidEncryptDecryptVector -> TestTree
testInvalidDecrypt tv =
  testCase "Invalid Decrypt" $ do
    let conversationKey' = hexToByteString (invEncConversationKey tv)
    let payload' = invPayload tv

    case decrypt conversationKey' payload' of
      Left _ -> return ()
      Right _ -> assertFailure "Decryption succeeded unexpectedly"

loadTestVectors :: FilePath -> IO (Either String TestVectors)
loadTestVectors path = do
  result <- try (BS.readFile path) :: IO (Either SomeException ByteString)
  case result of
    Left e -> return $ Left $ "Error reading file: " ++ show e
    Right content -> return $ eitherDecode $ LBS.fromStrict content

createTestGroup :: String -> [a] -> (a -> TestTree) -> TestTree
createTestGroup label vectors testFn =
  testGroup label (map testFn vectors)

-- Main test function
tests :: IO TestTree
tests = do
  let filePath = "test/Nostr/nip44.vectors.json"

  fileExists <- doesFileExist filePath
  if fileExists
    then do
      vectorsResult <- loadTestVectors filePath
      case vectorsResult of
        Left err -> return $ testCase "Loading Failure" $ assertFailure err
        Right vs -> return $ testGroup "Nostr Encryption Tests"
          [ createTestGroup "Get Conversation Key" (validGetConversationKey $ valid (v2 vs)) testGetConversationKey
          , createTestGroup "Get Message Keys" [validGetMessageKeys $ valid (v2 vs)] testGetMessageKeys
          , createTestGroup "Calc Padded Length" (calcPaddedLength $ valid (v2 vs)) testCalcPaddedLength
          , createTestGroup "Valid Encrypt / Decrypt" (encryptDecrypt $ valid (v2 vs)) testValidEncryptDecrypt
          , createTestGroup "Valid Encrypt / Decrypt Long Msg" (encryptDecryptLongMsg $ valid (v2 vs)) testValidEncryptDecryptLongMsg
          , createTestGroup "Invalid Message Lengths" (encryptMsgLengths $ invalid (v2 vs)) testInvalidMessageLengths
          , createTestGroup "Invalid Get Conversation Keys" (invalidGetConversationKey $ invalid (v2 vs)) testInvalidGetConversationKey
          , createTestGroup "Invalid Decrypt" (invalidDecrypt $ invalid (v2 vs)) testInvalidDecrypt
          ]
    else return $ testCase "File Missing" $ assertFailure $ "Could not find " ++ filePath
