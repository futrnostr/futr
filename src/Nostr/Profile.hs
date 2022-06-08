{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Profile where

import           Crypto.Schnorr         (KeyPair, SchnorrSig, XOnlyPubKey)
import qualified Crypto.Schnorr         as Schnorr
import           Data.Aeson
import           Data.Default
import           Data.Text              (Text, pack)
import qualified Data.Vector            as V
import           GHC.Exts               (fromList)

type RelayURL = Text

data Profile = Profile
  { name        :: Text
  , displayName :: Text
  , about       :: Text
  , picture     :: Text
  }
  deriving (Eq, Show)

instance Default Profile where
  def = Profile "" "" "" ""


instance FromJSON Profile where
  parseJSON = withObject "profile" $ \e -> Profile
    <$> e .: "name"
    <*> e .: "display_name"
    <*> e .: "about"
    <*> e .: "picture"


instance ToJSON Profile where
  toJSON (Profile name displayName about picture) =
    object $ fromList
      [ ( "name", String name )
      , ( "display_name", String displayName )
      , ( "about", String about )
      , ( "picture", String picture )
      ]
