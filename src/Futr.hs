{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell   #-}

module Futr where

import Control.Lens
import Crypto.Schnorr (XOnlyPubKey)
import Data.DateTime
import Data.Default
import Data.List (find, nub, sortBy)
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
  , _events           :: [ReceivedEvent]
  , _contacts         :: [XOnlyPubKey]
  , _profiles         :: Map XOnlyPubKey (Profile, DateTime)
  }
  deriving (Eq, Show)

instance Default FutrModel where
  def = FutrModel (fromSeconds 0) Nothing [] [] Map.empty

makeLenses 'FutrModel

updateContacts :: [XOnlyPubKey] -> [(XOnlyPubKey, (Profile, DateTime))] -> [XOnlyPubKey]
updateContacts oldContacts newContactData = nub $ oldContacts ++ newContacts
  where
    newContacts = map (\(xo, _) -> xo) newContactData

updateProfiles :: Map XOnlyPubKey (Profile, DateTime) -> [(XOnlyPubKey, (Profile, DateTime))] -> Map XOnlyPubKey (Profile, DateTime)
updateProfiles original new =
  Map.union newContacts original
  where
    newContacts = Map.fromList $ catMaybes $ map (checkProfileIsNewer original) new

checkProfileIsNewer :: Map XOnlyPubKey (Profile, DateTime) -> (XOnlyPubKey, (Profile, DateTime)) -> Maybe (XOnlyPubKey, (Profile, DateTime))
checkProfileIsNewer original (xo, (p, d)) =
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

pictureUrl :: Map XOnlyPubKey (Profile, DateTime) -> XOnlyPubKey -> Maybe Text
pictureUrl profiles xo = do
  ((Profile _ _ _ p), _) <- Map.lookup xo profiles
  p