-- | Module: Nostr.Profile
-- Defines types and instances for profiles in the Nostr system.

{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Profile where

import           Data.Aeson
import           Data.Default
import           Data.Text              (Text)

type RelayURL = Text

type Username = Text

type DisplayName = Text

type About = Text

type Picture = Text

data Profile = Profile Username (Maybe DisplayName) (Maybe About) (Maybe Picture)
  deriving (Eq, Show)

instance Default Profile where
  def = Profile "" Nothing Nothing Nothing

instance ToJSON Profile where
  toJSON (Profile username displayName about picture) = object
    [ "name" .= toJSON username
    , "display_name" .= toJSON displayName
    , "about" .= toJSON about
    , "picture" .= toJSON picture
    ]

instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .: "name"
    <*> e .:? "display_name"
    <*> e .:? "about"
    <*> e .:? "picture"
