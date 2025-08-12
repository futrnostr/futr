module Nostr.FeedKeys
  ( feedKeyForPosts
  , feedKeyForMentions
  , feedKeyForComments
  , feedKeyForMentionsHex
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Nostr.Keys (PubKeyXO, exportPubKeyXO, byteStringToHex)

-- | Derive a stable feed key for a set of pubkeys for posts timeline anchoring
feedKeyForPosts :: [PubKeyXO] -> Text
feedKeyForPosts xs =
  let hexes = map (byteStringToHex . exportPubKeyXO) xs
      preview = take 5 hexes
  in T.intercalate ":" ("posts" : T.pack (show (length xs)) : preview)

-- | Key for mentions feed of a single user
feedKeyForMentions :: PubKeyXO -> Text
feedKeyForMentions pk =
  let h = byteStringToHex (exportPubKeyXO pk)
  in T.intercalate ":" ["mentions", h]

-- Helper when we only have hex pubkey strings from tags
feedKeyForMentionsHex :: Text -> Text
feedKeyForMentionsHex h = T.intercalate ":" ["mentions", h]

-- | Key for comments feed for a specific root event
feedKeyForComments :: Text -> Text
feedKeyForComments rootEventHex = T.intercalate ":" ["comments", rootEventHex]


