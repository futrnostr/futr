-- | Module: Nostr.Nip05Search
-- NIP-05 search functionality for finding users by DNS-based internet identifiers

module Nostr.Nip05Search
  ( Nip05SearchResult(..)
  , searchNip05
  , isNip05Identifier
  , parseNip05Identifier
  ) where

import Control.Exception (try, SomeException)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, splitOn, unpack, isInfixOf)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.Wreq (Response, getWith, defaults, param, responseBody)

import Nostr.Keys (PubKeyXO, pubKeyXOFromHex)

-- | Result of a NIP-05 search
data Nip05SearchResult = Nip05SearchResult
  { nip05Identifier :: Text
  , userPubKey :: PubKeyXO
  , relayHints :: [Text]  -- Optional relay recommendations
  } deriving (Show, Eq, Generic)

-- | Response from .well-known/nostr.json
data Nip05Response = Nip05Response
  { names :: Maybe Object
  , relays :: Maybe Object
  } deriving (Show)

instance FromJSON Nip05Response where
  parseJSON = withObject "Nip05Response" $ \v -> Nip05Response
    <$> v .:? "names"
    <*> v .:? "relays"

-- | Check if a text input looks like a NIP-05 identifier (email-like format)
isNip05Identifier :: Text -> Bool
isNip05Identifier input =
  let trimmed = T.strip input
  in "@" `isInfixOf` trimmed &&
     not ("nprofile" `T.isPrefixOf` trimmed) &&
     not ("npub" `T.isPrefixOf` trimmed) &&
     not ("note" `T.isPrefixOf` trimmed) &&
     not ("nevent" `T.isPrefixOf` trimmed) &&
     not ("naddr" `T.isPrefixOf` trimmed) &&
     case splitOn "@" trimmed of
       [localPart, domain] -> not (T.null localPart) && not (T.null domain)
       _ -> False

-- | Parse a NIP-05 identifier into local part and domain
parseNip05Identifier :: Text -> Maybe (Text, Text)
parseNip05Identifier input = case splitOn "@" (T.strip input) of
  [localPart, domain] | not (T.null localPart) && not (T.null domain) -> 
    Just (localPart, domain)
  _ -> Nothing

-- | Search for a user by NIP-05 identifier
searchNip05 :: Text -> IO (Maybe Nip05SearchResult)
searchNip05 identifier = case parseNip05Identifier identifier of
  Nothing -> return Nothing
  Just (localPart, domain) -> do
    let opts = defaults & param "name" .~ [localPart]
        url = "https://" ++ unpack domain ++ "/.well-known/nostr.json"

    result <- try (getWith opts url) :: IO (Either SomeException (Response ByteString))
    case result of
      Left _ -> return Nothing
      Right response -> do
        let body = response ^. responseBody
        case decode body of
          Just nip05Response -> extractSearchResult identifier localPart nip05Response
          Nothing -> return Nothing

-- | Extract search result from NIP-05 response
extractSearchResult :: Text -> Text -> Nip05Response -> IO (Maybe Nip05SearchResult)
extractSearchResult identifier localPart nip05Response = case names nip05Response of
  Nothing -> return Nothing
  Just nameMap -> case KeyMap.lookup (Key.fromText localPart) nameMap of
    Just (String pubKeyHex) -> case pubKeyXOFromHex pubKeyHex of
      Just pubKey -> do
        let relaysList = extractRelaysForPubKey pubKeyHex (relays nip05Response)
        return $ Just $ Nip05SearchResult
          { nip05Identifier = identifier
          , userPubKey = pubKey
          , relayHints = relaysList
          }
      Nothing -> return Nothing
    _ -> return Nothing

-- | Extract relay recommendations for a public key
extractRelaysForPubKey :: Text -> Maybe Object -> [Text]
extractRelaysForPubKey pubKeyHex maybeRelaysObj = case maybeRelaysObj of
  Nothing -> []
  Just relaysObj -> case KeyMap.lookup (Key.fromText pubKeyHex) relaysObj of
    Just (Array relayArray) -> map extractRelayText (toList relayArray)
    _ -> []
  where
    extractRelayText (String relayUri) = relayUri
    extractRelayText _ = ""

    toList = foldr (:) []
