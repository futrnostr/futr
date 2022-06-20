{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell   #-}

module Futr where

import Control.Lens
import Crypto.Schnorr (XOnlyPubKey)
import Data.DateTime
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Nostr.Event
import Nostr.Keys
import Nostr.Profile

data FutrModel = FutrModel
  { _time             :: DateTime
  , _selectedKeys     :: Keys
  -- nostr data
  , _contacts         :: Map XOnlyPubKey (Profile, DateTime)
  , _events           :: [ReceivedEvent]
  , _profiles         :: Map XOnlyPubKey (Profile, DateTime)
  }
  deriving (Eq, Show)

instance Default FutrModel where
  def = FutrModel (fromSeconds 0) initialKeys Map.empty [] Map.empty

makeLenses 'FutrModel
