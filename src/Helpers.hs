{-# LANGUAGE OverloadedStrings    #-}

module Helpers where

import           Crypto.Schnorr
import           Data.Aeson
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.DateTime
import           Data.List                            (sortBy)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text, pack)
import           Data.Text.Encoding                   (encodeUtf8)

import           NostrTypes

mainKeys :: [Keys] -> Keys
mainKeys ks = head $ filter (\(Keys _ _ xo _) -> xo == True) ks

poolWithoutRelay :: [Relay] -> Relay -> [Relay]
poolWithoutRelay p r = filter (\r' -> not $ r `sameRelay` r') p

sameRelay :: Relay -> Relay -> Bool
sameRelay a b = host a == host b && port a == port b

sortPool :: Relay -> Relay -> Ordering
sortPool a b = mconcat [compare (host a) (host b), compare (port a) (port b)]

profileDataFromReceivedEvents :: [ReceivedEvent] -> XOnlyPubKey -> Maybe ProfileData
profileDataFromReceivedEvents res xo = profileData
  where
    relatedEvents = filter
      (\re ->
        kind (fst re) == 0
        && NostrTypes.pubKey (fst re) == xo
      ) res
    profileData = case null relatedEvents of
      True -> Nothing
      False -> decode $ LazyBytes.fromStrict $ encodeUtf8 $ content $ fst $ head $ sortBy latestEvent relatedEvents

latestEvent :: ReceivedEvent -> ReceivedEvent -> Ordering
latestEvent a b = compare (created_at $ fst b) (created_at $ fst a)

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
