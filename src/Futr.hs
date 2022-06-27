{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell   #-}

module Futr where

import Control.Lens
import Crypto.Schnorr (XOnlyPubKey)
import Data.DateTime
import Data.Default
import Data.List (filter, find, nub, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)

import Helpers
import Nostr.Event
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay
import Nostr.Response

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
      if d > d'
        then Just (xo, (p, d))
        else Nothing

addEvents :: [ReceivedEvent] -> [(Event, Relay)] -> [ReceivedEvent]
addEvents re [] = re
addEvents re ((e,r) : ers) = addEvents (addedEvent : newList) ers
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r `sameRelay` r') rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'

pictureUrl :: Map XOnlyPubKey (Profile, DateTime) -> XOnlyPubKey -> Maybe Text
pictureUrl profiles xo = do
  ((Profile _ _ _ p), _) <- Map.lookup xo profiles
  p

newFutr :: FutrModel -> [(Response, Relay)] -> FutrModel
newFutr model responseList = model
  & contacts .~ updateContacts (model ^. contacts) newContacts
  & profiles .~ updateProfiles (model ^. profiles) (newContacts ++ newProfiles)
  & events .~ sortBy sortByDate (addEvents (model ^. events) newTextNotes)
  where
    newContacts = nub $ concat $ map extractContacts $ filter filterContacts responseList
    newProfiles = catMaybes $ map extractProfile $ filter filterMetadata responseList
    newTextNotes = map extractTextNote $ filter filterTextNotes responseList
    sortByDate a b = compare (created_at $ fst b) (created_at $ fst a)

extractContacts :: (Response, Relay) -> [(XOnlyPubKey, (Profile, DateTime))]
extractContacts (EventReceived _ event, relay) = catMaybes $ map (tagToProfile $ created_at event) (tags event)
extractContacts _ = error "this should not be possible, contact events are filtered first"

extractProfile :: (Response, Relay) -> Maybe (XOnlyPubKey, (Profile, DateTime))
extractProfile (EventReceived _ event, relay) = parseProfiles event
  where
    parseProfiles e = case readProfile e of
      Just p -> Just (pubKey e, (p, created_at e))
      Nothing -> Nothing
extractProfile _ = error "this should not be possible, contact and metadata events are filtered first"

extractTextNote :: (Response, Relay) -> (Event, Relay)
extractTextNote (EventReceived _ event, relay) = (event, relay)
extractTextNote _ = error "this should not be possible, text note events are filtered first"

filterContacts :: (Response, Relay) -> Bool
filterContacts response =
  case response of
    (EventReceived _ event, relay') ->
      case kind event of
        Contacts -> True
        _ -> False
    _ -> False

filterMetadata :: (Response, Relay) -> Bool
filterMetadata response =
  case response of
    (EventReceived _ event, relay') ->
      case kind event of
        Metadata -> True
        _ -> False
    _ -> False

filterTextNotes :: (Response, Relay) -> Bool
filterTextNotes response =
  case response of
    (EventReceived _ event, relay') ->
      case kind event of
        TextNote -> True
        _ -> False
    _ -> False
