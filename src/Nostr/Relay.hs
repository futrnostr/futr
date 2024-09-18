-- | Module: Nostr.Relay
-- Defines functions related to relays in the Nostr protocol.

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nostr.Relay where

import Basement.IntegralConv (wordToInt)
import Control.Lens ((^.), (^?), (<&>), _Right)

import Data.Function (on)
import Data.Text (Text)


import Text.URI (render)
import Text.URI.Lens (uriAuthority, uriPath, uriScheme, authHost, authPort)
import qualified Text.URI as URI
import qualified Text.URI.QQ as QQ

import Nostr.Types (Relay(..), RelayInfo(..), RelayURI(..), unwrapRelayURI)

-- | Provides a default list of relays.
defaultRelays :: [Relay]
defaultRelays =
  [ Relay (RelayURI [QQ.uri|wss://nostr.rocks|]) (RelayInfo True True)
  , Relay (RelayURI [QQ.uri|ws://localhost:2700|]) (RelayInfo True True)
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

