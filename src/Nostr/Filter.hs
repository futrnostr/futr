{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Filter where

import Data.Aeson
import Data.Text (Text, pack)
import qualified Data.Vector as V
import GHC.Exts (fromList)

import Nostr.Event
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile

data Filter
  = MetadataFilter [PubKeyXO] Int
  | FollowListFilter [PubKeyXO] Int
  | ShortTextNoteFilter [PubKeyXO] Int
  | LinkedEvents [EventId] Int
  | AllNotes Int
  | AllMetadata Int
  deriving (Eq, Show)

instance ToJSON Filter where
  toJSON (MetadataFilter xos now) = object
    [ "kinds" .= toJSON [Metadata]
    , "authors" .= toJSON xos
    , "limit" .= Number 1
    , "until" .= toJSON (now + 60)
    ]
  toJSON (FollowListFilter xos now) = object
    [ "kinds" .= toJSON [FollowList]
    , "authors" .= toJSON xos
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]
  toJSON (ShortTextNoteFilter xos now) = object
    [ "kinds" .= toJSON [ShortTextNote, EventDeletion]
    , "authors" .= toJSON xos
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]
  toJSON (LinkedEvents eids now) = object
    [ "kinds" .= toJSON [ShortTextNote]
    , "limit" .= Number 500
    , "#e" .= toJSON eids
    ]
  toJSON (AllNotes now) = object
    [ "kinds" .= toJSON [ShortTextNote]
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]
  toJSON (AllMetadata now) = object
    [ "kinds" .= toJSON [Metadata]
    , "limit" .= Number 500
    , "until" .= toJSON (now + 60)
    ]
