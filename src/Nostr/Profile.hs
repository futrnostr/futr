{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Profile where

import           Crypto.Schnorr         (KeyPair, SchnorrSig, XOnlyPubKey)
import qualified Crypto.Schnorr         as Schnorr
import           Data.Aeson
import           Data.Default
import           Data.Text              (Text, pack)
import qualified Data.Vector            as V
import           GHC.Exts               (fromList)

import Nostr.Keys

type RelayURL = Text

data ProfileData = ProfileData
  { name       :: Text
  , about      :: Text
  , pictureUrl :: Text
  , nip05      :: Text
  }
  deriving (Eq, Show)

data Profile = Profile XOnlyPubKey RelayURL ProfileData
  deriving (Eq, Show)

instance Default ProfileData where
  def = ProfileData "" "" "" ""

instance FromJSON ProfileData where
  parseJSON = withObject "profile data" $ \e -> ProfileData
    <$> e .: "name"
    <*> e .: "about"
    <*> e .: "picture"
    <*> e .: "nip05"

instance FromJSON Profile where
  parseJSON (Array v)
    | V.length v == 3 =
      case v V.! 0 of
        String "p" ->
          Profile <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2) <*> parseJSON ""
        _ -> fail "Unknown profile"
    | V.length v == 4 && v V.! 0 == String "p" =
        Profile <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2) <*> parseJSON (v V.! 3)
    | otherwise = fail "Invalid profile"
  parseJSON _ = fail "Cannot parse profile"

instance ToJSON Profile where
  toJSON (Profile xo relayURL pd) =
    Array $ fromList
      [ String "p"
      , String $ pack $ Schnorr.exportXOnlyPubKey xo
      , String relayURL
      , String $ name pd
      ]
