-- | Module: Nostr.Profile
-- Defines types and instances for profiles in the Nostr protocol.

{-# LANGUAGE DeriveGeneric #-}

module Nostr.Profile
  ( Profile(..)
  , emptyProfile
  , verifyNip05 -- @todo move to effect system
  )
  where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Lens ((&), (.~), (^.))
import Control.Exception (try, SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, splitOn, unpack)
import GHC.Generics (Generic)
import Network.Wreq (Response, getWith, defaults, param, responseBody)

import Nostr.Keys (PubKeyXO)

-- | Represents a user profile.
data Profile = Profile
  { name :: Maybe Text
  , displayName :: Maybe Text
  , about :: Maybe Text
  , picture :: Maybe Text
  , nip05 :: Maybe Text
  , banner :: Maybe Text
  } deriving (Eq, Generic, Show)


-- | Empty profile.
emptyProfile :: Profile
emptyProfile = Profile Nothing Nothing Nothing Nothing Nothing Nothing


data Nip05Response = Nip05Response
  { names :: Maybe Object
  } deriving (Show)



-- | 'ToJSON' instance for 'Profile'.
instance ToJSON Profile where
  toEncoding (Profile n dn abt pic nip bnr) = pairs $
    "name" .= n <>
    "display_name" .= dn <>
    "about" .= abt <>
    "picture" .= pic <>
    "nip05" .= nip <>
    "banner" .= bnr


-- | 'FromJSON' instance for 'Profile'.
instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .:? "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
    <*> e .:? "nip05"
    <*> e .:? "banner"


instance FromJSON Nip05Response where
  parseJSON = withObject "Nip05Response" $ \v -> Nip05Response <$> v .:? "names"


verifyNip05 :: Profile -> PubKeyXO -> IO Bool
verifyNip05 p pubkey = case nip05 p of
  Just n -> do
    let (localPart, domain) = parseNip05 n
    let opts = defaults & param "name" .~ [localPart]
    result <- try (getWith opts ("https://" ++ unpack domain ++ "/.well-known/nostr.json")) :: IO (Either SomeException (Response ByteString))
    return $ either (const False) (checkResponse localPart (pack $ show pubkey) . (^. responseBody)) result
  Nothing -> return False


checkResponse :: Text -> Text -> ByteString -> Bool
checkResponse localPart pubkey body =
  case decode body of
    Just (Nip05Response (Just nameMap)) ->
      case KeyMap.lookup (Key.fromText localPart) nameMap of
        Just (String key) -> key == pubkey
        _ -> False
    _ -> False


parseNip05 :: Text -> (Text, Text)
parseNip05 n = case splitOn "@" n of
  [localPart, domain] -> (localPart, domain)
  _ -> ("", "")
