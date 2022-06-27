{-# LANGUAGE OverloadedStrings    #-}

module Helpers where

import Crypto.Schnorr (XOnlyPubKey, exportXOnlyPubKey)
import Data.DateTime
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Monomer
import Nostr.Event
import Nostr.Keys
import Nostr.Profile

voidTask :: IO () -> EventResponse s e sp ep
voidTask action = Producer (const action)

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
  $ part1 ++ ".." ++ (reverse part2)
  where
    str = exportXOnlyPubKey xo
    part1 = take 4 str
    part2 = take 4 $ reverse str

middleXOnlyPubKey :: XOnlyPubKey -> Text
middleXOnlyPubKey xo = pack
  $ part1 ++ ".." ++ (reverse part2)
  where
    str = exportXOnlyPubKey xo
    part1 = take 8 str
    part2 = take 8 $ reverse str

tagToProfile :: DateTime -> Tag -> Maybe (XOnlyPubKey, (Profile, DateTime))
tagToProfile datetime (PTag (ValidXOnlyPubKey xo) _ name) = Just (xo,  ( Profile (fromMaybe "" name) Nothing Nothing Nothing, datetime))
tagToProfile _ _ = Nothing
