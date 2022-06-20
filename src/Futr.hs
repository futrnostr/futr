{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell   #-}

module Futr where

import Control.Lens
import Crypto.Schnorr (XOnlyPubKey)
import Data.DateTime
import Data.Default
import Data.List (find, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)

import Nostr.Event
import Nostr.Keys
import Nostr.Profile
import Nostr.Relay

data FutrModel = FutrModel
  { _time             :: DateTime
  , _selectedKeys     :: Maybe Keys
  -- nostr data
  , _contacts         :: Map XOnlyPubKey (Profile, DateTime)
  , _events           :: [ReceivedEvent]
  , _profiles         :: Map XOnlyPubKey (Profile, DateTime)
  }
  deriving (Eq, Show)

instance Default FutrModel where
  def = FutrModel (fromSeconds 0) Nothing Map.empty [] Map.empty

makeLenses 'FutrModel

updateContacts :: Map XOnlyPubKey (Profile, DateTime) -> [(XOnlyPubKey, (Profile, DateTime))] -> Map XOnlyPubKey (Profile, DateTime)
updateContacts original new =
  Map.union newContacts original
  where
    newContacts = Map.fromList $ catMaybes $ map (checkContactIsNewer original) new

checkContactIsNewer :: Map XOnlyPubKey (Profile, DateTime) -> (XOnlyPubKey, (Profile, DateTime)) -> Maybe (XOnlyPubKey, (Profile, DateTime))
checkContactIsNewer original (xo, (p, d)) =
  case Map.lookup xo original of
    Nothing ->
      Just (xo, (p, d))
    Just (p', d') ->
      if d' > d
        then Just (xo, (p', d'))
        else Nothing

addEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r `sameRelay` r') rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'
    sortByDate a b = compare (created_at $ fst b) (created_at $ fst a)
