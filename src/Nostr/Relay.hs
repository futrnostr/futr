-- | Module: Nostr.Relay
-- Defines types and functions related to relays in the Nostr system.

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nostr.Relay where

import Basement.IntegralConv (wordToInt)
import Control.Lens ((^.), (^?), (<&>), _Right)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, withText, (.:), Value(..), object)
import Data.Function (on)
import Data.Text (Text)
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import Text.URI (URI, mkURI, render)
import Text.URI.Lens (uriAuthority, uriPath, uriScheme, authHost, authPort)
import qualified Text.URI as URI
import qualified Text.URI.QQ as QQ

-- | Represents a wrapped URI used within a relay.
newtype RelayURI = RelayURI URI deriving (Eq, Ord, Show)

-- | Represents the information associated with a relay.
data RelayInfo = RelayInfo
  { readable  :: Bool
  , writable  :: Bool
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | Represents a relay entity containing URI, relay information, and connection status.
data Relay = Relay
  { uri       :: RelayURI
  , info      :: RelayInfo
  , connected :: Bool
  }
  deriving (Eq, Show)

-- | Converts a `RelayURI` into its JSON representation.
instance FromJSON RelayURI where
  parseJSON = withText "URI" $ \u ->
    case mkURI u of
      Just u' -> return $ RelayURI u'
      Nothing -> fail "Invalid relay URI"

-- | Parses a JSON value into a `RelayURI`.
instance ToJSON RelayURI where
  toJSON (RelayURI u) = String $ render u

-- | Instance for ordering 'Relay' values based on their 'uri'.
instance Ord Relay where
  compare (Relay r _ _) (Relay r' _ _) = compare r r'

-- | Instance for parsing a 'Relay' from JSON.
instance FromJSON Relay where
  parseJSON = withObject "Relay" $ \r -> do
    uri'  <- r .: "uri"
    info' <- r .: "info"
    return $ Relay uri' info' False

-- | Instance for converting a 'Relay' to JSON.
instance ToJSON Relay where
  toJSON r = object $ fromList
    [ ( "uri", String $ render $ unwrapRelayURI $ uri r)
    , ( "info", toJSON $ info r)
    ]

-- | Provides a default list of relays.
defaultRelays :: [(RelayURI, RelayInfo)]
defaultRelays =
  [ (RelayURI [QQ.uri|wss://nostr.rocks|], RelayInfo True True)
  , (RelayURI [QQ.uri|ws://localhost:2700|], RelayInfo True True)
  ]

-- | Retrieves the textual representation of the relay's URI.
relayName :: Relay -> Text
relayName r = render $ unwrapRelayURI $ uri r

-- | Extracts the scheme of a relay's URI.
extractScheme :: Relay -> Maybe Text
extractScheme r = uri' ^. uriScheme <&> URI.unRText
  where
    uri' = unwrapRelayURI $ uri r

-- | Extracts the hostname of a relay's URI.
extractHostname :: Relay -> Maybe Text
extractHostname r = uri' ^? uriAuthority . _Right . authHost <&> URI.unRText
  where
    uri' = unwrapRelayURI $ uri r

-- | Extracts the port of a relay's URI.
extractPort :: Relay -> Int
extractPort r =
  case uri' ^? uriAuthority . _Right . authPort of
    Just (Just p) -> wordToInt p
    _ -> if extractScheme r == Just "wss" then 443 else 80
  where
    uri' = unwrapRelayURI $ uri r

-- | Extracts the path of a relay's URI.
extractPath :: Relay -> Text
extractPath r =
  case uri' ^? uriPath of
    Just [] -> "/"
    Just p  -> foldMap ("/" <>) (map URI.unRText p)
    _       -> "/"
  where
    uri' = unwrapRelayURI $ uri r

-- | Checks if two relays are the same based on URI.
sameRelay :: Relay -> Relay -> Bool
sameRelay = (==) `on` uri

-- | Remove a relay from a list of relays
removeRelayFromList :: [Relay] -> Relay -> [Relay]
removeRelayFromList relays relayToRemove = filter (not . sameRelay relayToRemove) relays

-- | Unwrap relay URI.
unwrapRelayURI :: RelayURI -> URI
unwrapRelayURI (RelayURI u) = u
