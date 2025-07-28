-- | Module: Nostr.Types
-- Defines types related to the Nostr protocol.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Types where

import Control.Monad (when)
import Data.Aeson hiding (Error)
import Data.Aeson.Encoding (list, text, pair)
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Prelude hiding (until)

import Nostr.Event (Event, EventId(..), Kind, decodeHex)
import Nostr.Keys (PubKeyXO(..))

-- | Represents a subscription id as text.
type SubscriptionId = Text


-- | Represents a subscription.
data Subscription = Subscription
  { subId   :: SubscriptionId
  , filter :: Filter
  }
  deriving (Eq, Generic, Show)


-- | Represents a filter for events.
data Filter = Filter
  { ids     :: Maybe [EventId]
  , authors :: Maybe [PubKeyXO]
  , kinds   :: Maybe [Kind]
  , since   :: Maybe Int
  , until   :: Maybe Int
  , limit   :: Maybe Int
  , fTags   :: Maybe (Map.Map Char [Text])
  }
  deriving (Eq, Generic, Show)


-- | Empty filter.
emptyFilter :: Filter
emptyFilter = Filter
  { ids = Nothing
  , authors = Nothing
  , kinds = Nothing
  , since = Nothing
  , until = Nothing
  , limit = Nothing
  , fTags = Nothing
  }


-- | Represents a request to the relay.
data Request
  = SendEvent Event
  | Subscribe Subscription
  | Close SubscriptionId
  | Disconnect
  | Authenticate Event
  deriving (Eq, Generic, Show)


-- | Represents a response from the relay.
data Response
  = EventReceived SubscriptionId Event
  | Ok (Maybe EventId) Bool (Maybe Text)
  | Eose SubscriptionId
  | Closed SubscriptionId Text
  | Notice Text
  | Auth Text
  deriving (Eq, Show)


-- | Represents a standard prefix for error messages.
data StandardPrefix = Duplicate | Pow | Blocked | RateLimited | Invalid | Error
    deriving (Eq, Show)

-- Helper functions


-- | Converts an error message to a standard prefix.
noticeReason :: Text -> StandardPrefix
noticeReason errMsg
  | "duplicate:"    `T.isInfixOf` errMsg = Duplicate
  | "pow:"          `T.isInfixOf` errMsg = Pow
  | "blocked:"      `T.isInfixOf` errMsg = Blocked
  | "rate-limited:" `T.isInfixOf` errMsg = RateLimited
  | "invalid:"      `T.isInfixOf` errMsg = Invalid
  | otherwise                            = Error


-- | Converts a JSON array into a 'Response'.
instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0 :: Parser Text
    case type' of
      "EVENT" -> do
        subId' <- parseJSON $ arr V.! 1
        event <- parseJSON $ arr V.! 2
        return $ EventReceived subId' event
      "OK" -> do
        idStr <- parseJSON $ arr V.! 1 :: Parser Text
        bool <- parseJSON $ arr V.! 2
        message <- if V.length arr > 3
                    then Just <$> parseJSON (arr V.! 3)
                    else pure Nothing
        -- Convert empty string to Nothing, otherwise try to parse as EventId
        let eventId = if T.null idStr
                     then Nothing
                     else Just $ EventId $ fromMaybe "" $ decodeHex $ encodeUtf8 idStr
        return $ Ok eventId bool message
      "EOSE" -> do
        when (V.length arr < 2) $
          fail "Malformed EOSE message: missing subscription ID"
        subId' <- parseJSON $ arr V.! 1
        return $ Eose subId'
      "CLOSED" -> do
        subId' <- parseJSON $ arr V.! 1
        message <- parseJSON $ arr V.! 2
        return $ Closed subId' message
      "NOTICE" -> do
        message <- parseJSON $ arr V.! 1
        return $ Notice message
      "AUTH" -> do
        challenge <- parseJSON $ arr V.! 1
        return $ Auth challenge
      _ -> fail "Unknown response type"


-- | Converts a 'Subscription' to its JSON representation.
instance ToJSON Subscription where
  toEncoding (Subscription efs s) = pairs $ "subId" .= s <> "filter" .= efs


-- | Converts a 'Request' to its JSON representation.
instance ToJSON Request where
  toEncoding req = case req of
    Authenticate event -> list id [text "AUTH", toEncoding event]
    SendEvent event -> list id [text "EVENT", toEncoding event]
    Subscribe (Subscription subId f) -> list id $ text "REQ" : text subId : [ toEncoding f ]
    Close subId -> list text ["CLOSE", subId]
    Disconnect -> list text ["DISCONNECT"]


-- | 'ToJSON' instance for 'Filter'.
instance ToJSON Filter where
  toEncoding Filter {..} = pairs $ mconcat $ catMaybes
    [ fmap (pair "ids" . toEncoding) ids
    , fmap (pair "authors" . toEncoding) authors
    , fmap (pair "kinds" . toEncoding) kinds
    , fmap (pair "since" . toEncoding) since
    , fmap (pair "until" . toEncoding) until
    , fmap (pair "limit" . toEncoding) limit
    ] ++ maybe [] (pure . encodeTags) fTags
    where
      encodeTags :: Map.Map Char [Text] -> Series
      encodeTags = Map.foldrWithKey (\k v acc -> acc <> pair (fromText ("#" <> T.singleton k)) (toEncoding v)) mempty
