{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Filter where

import           Crypto.Schnorr
import           Data.Aeson
import           Data.DateTime
import           Data.Text              (Text, pack)
import qualified Data.Vector            as V
import           GHC.Exts               (fromList)

import Nostr.Event
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile

data Filter
  = MetadataFilter [XOnlyPubKey] DateTime
  | ContactsFilter [XOnlyPubKey] DateTime
  | TextNoteFilter [XOnlyPubKey] DateTime
  | LinkedEvents [EventId] DateTime
  | AllNotes DateTime
  | AllMetadata DateTime
  deriving (Eq, Show)

instance ToJSON Filter where
  toJSON (MetadataFilter xos now) =
    object $ fromList
      [ ( "kinds", toJSON [ Metadata ] )
      , ( "authors", toJSON xos )
      , ( "limit", Number 1 )
      , ( "until", toJSON $ (toSeconds now + 60))
      ]
  toJSON (ContactsFilter xos now) =
    object $ fromList
      [ ( "kinds", toJSON [ Contacts ] )
      , ( "authors", toJSON xos )
      , ( "limit", Number 500 )
      , ( "until", toJSON $ (toSeconds now + 60))
      ]
  toJSON (TextNoteFilter xos now) =
    object $ fromList
      [ ( "kinds", toJSON [ TextNote, Delete ] )
      , ( "authors", toJSON xos )
      , ( "limit", Number 500 )
      , ( "until", toJSON $ (toSeconds now + 60))
      ]
  toJSON (LinkedEvents eids now) =
    object $ fromList
      [ ( "kinds", toJSON [ TextNote ] )
      , ( "limit", Number 500 )
      , ( "#e", toJSON eids )
      , ( "until", toJSON $ (toSeconds now + 60))
      ]
  toJSON (AllNotes now) =
    object $ fromList
      [ ( "kinds", toJSON [ TextNote ] )
      , ( "limit", Number 500 )
      , ( "until", toJSON $ (toSeconds now + 60))
      ]
  toJSON (AllMetadata now) =
    object $ fromList
      [ ( "kinds", toJSON [ Metadata ] )
      , ( "limit", Number 500 )
      , ( "until", toJSON $ (toSeconds now + 60))
      ]
