{-# LANGUAGE OverloadedStrings    #-}

module Helpers where

import Crypto.Schnorr                 (XOnlyPubKey, exportXOnlyPubKey)
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.List                      (sortBy)
import Data.Maybe                     (fromMaybe)
import Data.Text                      (Text, pack)
import Data.Text.Encoding             (encodeUtf8)
import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as LazyBytes

import Nostr.Event
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay

-- mainKeys :: [Keys] -> Keys
-- mainKeys ks = head $ filter (\(Keys _ _ xo _) -> xo == True) ks


-- profileDataFromReceivedEvents :: [ReceivedEvent] -> XOnlyPubKey -> Maybe ProfileData
-- profileDataFromReceivedEvents res xo = profileData
--   where
--     relatedEvents = filter
--       (\re ->
--         kind (fst re) == Metadata
--         && pubKey (fst re) == xo
--       ) res
--     profileData = case null relatedEvents of
--       True -> Nothing
--       False -> decode $ LazyBytes.fromStrict $ encodeUtf8 $ content $ fst $ head $ sortBy latestEvent relatedEvents
--
-- latestEvent :: ReceivedEvent -> ReceivedEvent -> Ordering
-- latestEvent a b = compare (created_at $ fst b) (created_at $ fst a)

xTimeAgo :: DateTime -> DateTime -> Text
xTimeAgo old new
  | diff > (60*60*24) = pack $ (show $ diff `div` 60  `div` 60  `div` 24) ++ " days ago"
  | diff > (60*60) = pack $ (show $ diff `div` 60 `div` 60) ++ " hours ago"
  | diff > 60 = pack $ (show $ diff `div` 60) ++ " minutes ago"
  | diff <= 0 = "just now"
  | diff < 60 = pack $ (show diff) ++ " seconds ago"
  | otherwise = pack $ show old
  where
    diff = toSeconds new - toSeconds old

shortXOnlyPubKey :: XOnlyPubKey -> Text
shortXOnlyPubKey xo = pack
  $ part1 ++ ".." ++ part2
  where
    str = exportXOnlyPubKey xo
    part1 = take 4 str
    part2 = take 4 $ reverse str


extractXOFromProfile :: Profile -> XOnlyPubKey
extractXOFromProfile (Profile xo _ _) = xo

profileName :: [Profile] -> XOnlyPubKey -> Text
profileName profiles xo =
  if null pList
    then ""
    else do
      let (Profile _ _ (ProfileData name _ _ _)) = head pList
      name
  where
    pList = filter (\(Profile xo' _ _) -> xo == xo') profiles

tagsToProfiles :: [Tag] -> [Profile]
tagsToProfiles ts = map (\t ->
    Profile (xo t) (fromMaybe "" $ r t) (pd t)
  ) ts'
  where
    ts' = filter (\p -> isPTag p) ts
    xo (PTag (ValidXOnlyPubKey xo) _ _) = xo
    xo _ = error "error transforming tags to profiles, invalid XOnlyPubKey"
    r (PTag _ r _) = r
    r _ = Nothing
    pd (PTag _ _ n) = ProfileData (fromMaybe "" n) "" "" ""
    pd _ = def

isPTag :: Tag -> Bool
isPTag (PTag (ValidXOnlyPubKey xo) _ _) = True
isPTag _ = False
