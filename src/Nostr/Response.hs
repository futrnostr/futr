{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Response where

import           Data.Aeson
import           Control.Monad          (mzero)
import           Data.Text              (Text)
import qualified Data.Vector            as V

import Nostr.Event
import Nostr.Relay
import Nostr.Request

data Response
  = EventReceived SubscriptionId Event
  | Notice Text
  deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0
    param <- parseJSON $ arr V.! 1
    case type' of
      String "EVENT"  -> do
        event <- parseJSON $ arr V.! 2
        return $ EventReceived param event
      String "NOTICE" -> return $ Notice param
      _ ->
        mzero

