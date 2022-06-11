{-# LANGUAGE OverloadedStrings    #-}

module Helpers where

import Control.Concurrent.MVar
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan
import Crypto.Schnorr (KeyPair, XOnlyPubKey, exportXOnlyPubKey)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.DateTime
import Data.Default
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Monomer

import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as LazyBytes

import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response

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

loadImportedKeyData
  :: WidgetEvent ep
  => TChan Request
  -> MVar RelayPool
  -> Keys
  -> (Keys -> Profile -> DateTime -> ep)
  -> IO ep
loadImportedKeyData requestChannel poolMVar keys trigger = do
  let (Keys kp xo _ _) = keys
  responseChannel <- atomically newTChan
  subId <- subscribe poolMVar requestChannel [LoadMetadataFilter xo] responseChannel
  response <- atomically $ readTChan responseChannel
  case response of
    (EventReceived _ event) -> do
      case kind event of
        Metadata -> do
          unsubscribe poolMVar requestChannel subId
          case readProfile event of
            Just profile -> do
              return $ trigger keys profile (created_at event)
            Nothing ->
              return $ trigger keys (Profile "" Nothing Nothing Nothing) (created_at event)
        _ -> error "Unexpected event kind received when loading key data"
    _ ->
      error "Unexpected response received when loading key data"