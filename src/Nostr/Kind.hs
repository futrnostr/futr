module Nostr.Kind where

import           Control.Monad          (mzero)
import           Data.Aeson

data Kind
  = Metadata
  | TextNote
  | Contacts
  | Delete
  | Boost
  | Like
  deriving (Eq, Show)

instance FromJSON Kind where
  parseJSON = withScientific "kind" $ \k -> do
    case k of
      0 -> return Metadata
      1 -> return TextNote
      3 -> return Contacts
      5 -> return Delete
      6 -> return Boost
      7 -> return Like
      _ -> mzero

instance ToJSON Kind where
  toJSON Metadata = Number 0
  toJSON TextNote = Number 1
  toJSON Contacts = Number 3
  toJSON Delete   = Number 5
  toJSON Boost    = Number 6
  toJSON Like     = Number 7
