-- | Module: Nostr.Relay
-- Defines types related to Nostr relays.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Nostr.Relay where

import Data.Aeson hiding (Error)
import Data.Aeson.Encoding (list, text)
import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Network.URI (URI(..), parseURI, uriAuthority, uriRegName, uriScheme)
import Prelude hiding (until)
import Text.Read (readMaybe)

-- | Represents a relay URI.
type RelayURI = Text


-- | Represents a relay with its URI and type combined.
data Relay
  = InboxRelay RelayURI        -- Read-only relay
  | OutboxRelay RelayURI       -- Write-only relay
  | InboxOutboxRelay RelayURI  -- Both read and write (also for DM)
  deriving (Eq, Generic, Show)


-- | Instance for ordering 'Relay' values based on their URI.
instance Ord Relay where
  compare r r' = compare (getUri r) (getUri r')


-- | Instance for converting a 'Relay' to JSON.
instance ToJSON Relay where
  toJSON relay = case relay of
    InboxRelay uri -> toJSON ["r", uri, "read"]
    OutboxRelay uri -> toJSON ["r", uri, "write"]
    InboxOutboxRelay uri -> toJSON ["r", uri]
  toEncoding relay = case relay of
    InboxRelay uri -> list id [text "r", text uri, text "read"]
    OutboxRelay uri -> list id [text "r", text uri, text "write"]
    InboxOutboxRelay uri -> list id [text "r", text uri]


-- | Instance for parsing a 'Relay' from JSON.
instance FromJSON Relay where
  parseJSON = withArray "Relay" $ \arr -> do
    case V.toList arr of
      ["r", String uri, String "read"] ->
        return $ InboxRelay uri
      ["r", String uri, String "write"] ->
        return $ OutboxRelay uri
      ["r", String uri] ->
        return $ InboxOutboxRelay uri
      _ -> fail "Invalid relay format"



-- | Get the URI from a Relay
getUri :: Relay -> RelayURI
getUri (InboxRelay uri)        = uri
getUri (OutboxRelay uri)       = uri
getUri (InboxOutboxRelay uri)  = uri


-- | Check if a relay is inbox capable
isInboxCapable :: Relay -> Bool
isInboxCapable (InboxRelay _) = True
isInboxCapable (InboxOutboxRelay _) = True
isInboxCapable (OutboxRelay _) = False


-- | Check if a relay is outbox capable
isOutboxCapable :: Relay -> Bool
isOutboxCapable (OutboxRelay _) = True
isOutboxCapable (InboxOutboxRelay _) = True
isOutboxCapable (InboxRelay _) = False


-- | Check if a relay URI is valid
isValidRelayURI :: RelayURI -> Bool
isValidRelayURI uriText =
    case parseURI (T.unpack uriText) of
        Just uri ->
            let scheme = uriScheme uri
                authority = uriAuthority uri
            in (scheme == "ws:" || scheme == "wss:") &&
                maybe False (not . null . uriRegName) authority
        Nothing -> False

-- Relay Helper functions

-- | Provides a default list of general relays.
defaultGeneralRelays :: ([Relay], Int)
defaultGeneralRelays =
  ( [ InboxOutboxRelay "wss://nos.lol"
    --, InboxOutboxRelay "wss://relay.nostr.bg"
    , InboxOutboxRelay "wss://nostr.mom"
    , InboxOutboxRelay "wss://offchain.pub"
    , InboxRelay "wss://relay.damus.io"
    ],
    0
  )


-- | Provides a default list of DM relays.
defaultDMRelays :: ([RelayURI], Int)
defaultDMRelays =
  ( [ "wss://auth.nostr1.com" ], 0 )


-- | Extracts the scheme of a relay's URI.
extractScheme :: RelayURI -> Maybe Text
extractScheme u =
  case T.splitOn "://" u of
    (scheme:_) -> Just scheme
    _ -> Nothing


-- | Extracts the hostname of a relay's URI.
extractHostname :: RelayURI -> Maybe Text
extractHostname u =
  case T.splitOn "://" u of
    (_:rest:_) -> Just $ T.takeWhile (/= ':') $ T.dropWhile (== '/') rest
    _ -> Nothing


-- | Extracts the port of a relay's URI.
extractPort :: RelayURI -> Int
extractPort u =
  case T.splitOn ":" $ T.dropWhile (/= ':') $ T.dropWhile (/= '/') $ T.dropWhile (/= ':') u of
    (_:portStr:_) -> maybe (defaultPort scheme) id $ readMaybe $ T.unpack portStr
    _ -> defaultPort scheme
  where
    scheme = extractScheme u
    defaultPort (Just "wss") = 443
    defaultPort (Just "ws") = 80
    defaultPort _ = 443


-- | Extracts the path of a relay's URI.
extractPath :: RelayURI -> Text
extractPath u =
  case T.splitOn "://" u of
    (_:rest:_) ->
      let withoutHost = T.dropWhile (/= '/') rest
      in if T.null withoutHost then "/" else withoutHost
    _ -> "/"


-- | Checks if two relays are the same based on URI.
sameRelay :: Relay -> Relay -> Bool
sameRelay = (==) `on` getUri
