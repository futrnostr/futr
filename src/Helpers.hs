{-# LANGUAGE OverloadedStrings    #-}

module Helpers where

import           Crypto.Schnorr
import           Data.Aeson
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.List                            (sortBy)
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text, pack)
import           Data.Text.Encoding                   (encodeUtf8)

import           NostrTypes

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

mainKeys :: [Keys] -> Keys
mainKeys ks = head $ filter (\k -> third k == True) ks

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


profileName :: [ReceivedEvent] -> XOnlyPubKey -> Text
profileName res xo =
  maybe (pack $ exportXOnlyPubKey xo) pdName (profileDataFromReceivedEvents res xo)

shortXOnlyPubKey :: XOnlyPubKey -> Text
shortXOnlyPubKey xo = pack
  $ part1 ++ ".." ++ part2
  where
    str = exportXOnlyPubKey xo
    part1 = take 3 str
    part2 = take 3 $ reverse str


latestEvent :: ReceivedEvent -> ReceivedEvent -> Ordering
latestEvent a b = compare (created_at $ fst b) (created_at $ fst a)
