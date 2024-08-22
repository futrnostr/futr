{-# LANGUAGE OverloadedStrings #-}

module Nostr.Nip05Verifier (verifyNip05) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, splitOn, unpack)
import Control.Lens ((&), (.~), (^.))
import Control.Exception (try, SomeException)
import Network.Wreq (Response, getWith, defaults, param, responseBody)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

import Nostr.Keys (PubKeyXO)
import Nostr.Profile (Profile(..))

data Nip05Response = Nip05Response
  { names :: Maybe Object
  } deriving (Show)

instance FromJSON Nip05Response where
  parseJSON = withObject "Nip05Response" $ \v -> Nip05Response <$> v .:? "names"

verifyNip05 :: Profile -> PubKeyXO -> IO Bool
verifyNip05 (Profile _ _ _ _ (Just nip05)) pubkey = do
  let (localPart, domain) = parseNip05 nip05
  let opts = defaults & param "name" .~ [localPart]
  result <- try (getWith opts ("https://" ++ unpack domain ++ "/.well-known/nostr.json")) :: IO (Either SomeException (Response ByteString))
  return $ either (const False) (checkResponse localPart (pack $ show pubkey) . (^. responseBody)) result
verifyNip05 _ _ = return False

checkResponse :: Text -> Text -> ByteString -> Bool
checkResponse localPart pubkey body =
  case decode body of
    Just (Nip05Response (Just nameMap)) ->
      case KeyMap.lookup (Key.fromText localPart) nameMap of
        Just (String key) -> key == pubkey
        _ -> False
    _ -> False

parseNip05 :: Text -> (Text, Text)
parseNip05 nip05 = case splitOn "@" nip05 of
  [localPart, domain] -> (localPart, domain)
  _ -> ("", "")