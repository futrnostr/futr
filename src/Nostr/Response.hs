{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Response where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Text (Text, isInfixOf)
import qualified Data.Vector as V

import Nostr.Event
import Nostr.Request hiding (Close)

data Response
  = EventReceived SubscriptionId Event -- used to send events requested by clients.
  | Ok EventId Bool Text               -- used to indicate acceptance or denial of an EVENT message.
  | Eose SubscriptionId                -- used to indicate the end of stored events and the beginning of events newly received in real-time.
  | Close SubscriptionId Text          -- used to indicate that a subscription was ended on the server side.
  | Notice Text                        -- used to send human-readable error messages or other things to clients.
  deriving (Eq, Show)

data StandardPrefix = Duplicate | Pow | Blocked | RateLimited | Invalid | Error
    deriving (Eq, Show)

noticeReason :: Text -> StandardPrefix
noticeReason errMsg
  | "duplicate:"    `isInfixOf` errMsg = Duplicate
  | "pow:"          `isInfixOf` errMsg = Pow
  | "blocked:"      `isInfixOf` errMsg = Blocked
  | "rate-limited:" `isInfixOf` errMsg = RateLimited
  | "invalid:"      `isInfixOf` errMsg = Invalid
  | otherwise                          = Error

instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0 :: Parser Text
    case type' of
      "EVENT" -> do
        subId' <- parseJSON $ arr V.! 1
        event <- parseJSON $ arr V.! 2
        return $ EventReceived subId' event
      "OK" -> do
        id' <- parseJSON $ arr V.! 2
        bool <- parseJSON $ arr V.! 3
        message <- parseJSON $ arr V.! 4
        return $ Ok id' bool message
      "EOSE" -> do
        subId' <- parseJSON $ arr V.! 1
        return $ Eose subId'
      "CLOSE" -> do
        subId' <- parseJSON $ arr V.! 1
        message <- parseJSON $ arr V.! 2
        return $ Close subId' message
      "NOTICE" -> do
        message <- parseJSON $ arr V.! 1
        return $ Notice message
      _ -> fail "Unknown response type"
