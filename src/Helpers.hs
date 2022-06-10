{-# LANGUAGE OverloadedStrings    #-}

module Helpers where

import Crypto.Schnorr (KeyPair, XOnlyPubKey, decodeHex, exportXOnlyPubKey, secKey, keyPairFromSecKey, xOnlyPubKey)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.DateTime
import Data.Default
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as LazyBytes

import Nostr.Event
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\(Keys kp xo _ n) -> Keys kp xo False n) ks

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

initialKeys :: Keys
initialKeys = Keys kp xo True Nothing where
  kp = keyPairFromSecKey $ load "fef52b22d4568d9235ebf8a4f35dac54a4e748781441506e133532099dae0ded" secKey
  xo = load "134bdeaf23fe7078d94b2836dcb748e762073d4bc274a2c188a44a3fc29df31c" xOnlyPubKey
  load :: String -> (ByteString -> Maybe a) -> a
  load s f =
    case decodeHex s of
      Just bs ->
        case f bs of
          Just b -> b
          _      -> error "failed to load initial keys"
      Nothing -> error "failed to load initial keys"
