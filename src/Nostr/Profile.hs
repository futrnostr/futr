-- | Module: Nostr.Profile
-- Defines types and instances for profiles in the Nostr system.

{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Profile
  ( RelayURL
  , Username
  , DisplayName
  , About
  , Picture
  , Nip05
  , Profile(..)
  , verifyNip05
  )
  where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Lens ((&), (.~), (^.))
import Control.Exception (try, SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Text (Text, pack, splitOn, unpack)
import Network.Wreq (Response, getWith, defaults, param, responseBody)

import Nostr.Keys (PubKeyXO)

type RelayURL = Text
type Username = Text
type DisplayName = Text
type About = Text
type Picture = Text
type Nip05 = Text

data Profile = Profile Username (Maybe DisplayName) (Maybe About) (Maybe Picture) (Maybe Nip05)
  deriving (Eq, Show)

instance Default Profile where
  def = Profile "" Nothing Nothing Nothing Nothing

instance ToJSON Profile where
  toJSON (Profile username displayName about picture nip05) = object
    [ "name" .= toJSON username
    , "display_name" .= toJSON displayName
    , "about" .= toJSON about
    , "picture" .= toJSON picture
    , "nip05" .= toJSON nip05
    ]

instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .: "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
    <*> e .:? "nip05"

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
