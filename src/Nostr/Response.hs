{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Response where

import           Data.Aeson
import           Control.Monad          (mzero)
import           Data.Text              (Text)
import qualified Data.Vector            as V

import Nostr.Event
import Nostr.Relay

data Response
  = EventReceived Text Event
  | Notice Text
  deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    t <- parseJSON $ arr V.! 0
    s <- parseJSON $ arr V.! 1
    e <- parseJSON $ arr V.! 2
    case t of
      String "EVENT"  -> return $ EventReceived s e
      String "NOTICE" -> return $ Notice s
      _               -> mzero
