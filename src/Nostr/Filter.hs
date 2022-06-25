{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Filter where

import           Crypto.Schnorr
import           Data.Aeson
import           Data.DateTime
import           Data.Text              (Text, pack)
import qualified Data.Vector            as V
import           GHC.Exts               (fromList)

import Nostr.Keys
import Nostr.Kind
import Nostr.Profile

data Filter
  = MetadataFilter [XOnlyPubKey]
  | ContactsFilter [XOnlyPubKey]
  | TextNoteFilter [XOnlyPubKey]
  | AllNotes
  | AllMetadata
  deriving (Eq, Show)

instance ToJSON Filter where
  toJSON (MetadataFilter xos) =
    object $ fromList
      [ ( "kinds", toJSON [ Metadata ] )
      , ( "authors", toJSON xos )
      , ( "limit", Number 1 )
      ]
  toJSON (ContactsFilter xos) =
    object $ fromList
      [ ( "kinds", toJSON [ Contacts ] )
      , ( "authors", toJSON xos )
      , ( "limit", Number 500 )
      ]
  toJSON (TextNoteFilter xos) =
    object $ fromList
      [ ( "kinds", toJSON [ TextNote ] )
      , ( "authors", toJSON xos )
      , ( "limit", Number 500 )
      ]
  toJSON AllNotes =
    object $ fromList
      [ ( "kinds", toJSON [ TextNote ] )
      , ( "limit", Number 100 )
      ]
  toJSON AllMetadata =
    object $ fromList
      [ ( "kinds", toJSON [ Metadata ] )
      , ( "limit", Number 100 )
      ]
