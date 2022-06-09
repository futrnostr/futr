{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.BackupKeys where

import Control.Lens
import Crypto.Schnorr
import Data.Default
import Data.Maybe (fromJust)
import Data.Text (pack)
import Monomer

import Nostr.Keys
import UIHelpers

data BackupKeysModel = BackupKeysModel
  { _backupKeys :: Maybe Keys
  } deriving (Eq, Show)

instance Default BackupKeysModel where
  def = BackupKeysModel Nothing

makeLenses 'BackupKeysModel

backupKeysWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => ep
  -> ALens' sp BackupKeysModel
  -> WidgetNode sp ep
backupKeysWidget done model =
  composite
    "BackupKeysWidget"
    model
    (viewBackupKeys done)
    (\_ _ _ e -> [Report e])

viewBackupKeys
  :: (WidgetModel sp, WidgetEvent ep)
  => ep
  -> WidgetEnv sp ep
  -> BackupKeysModel
  -> WidgetNode sp ep
viewBackupKeys done wenv model =
  vstack
    [ hstack
        [ filler
        , bigLabel "Backup your keys"
        , filler
        ]
    , filler
    , label "Your public key"
    , spacer
    , selectableText $ pack $ exportXOnlyPubKey $ xo
    , spacer
    , label "Share your public key with your friends, so they can find you."
    , filler
    , separatorLine
    , filler
    , label "Your private key"
    , spacer
    , selectableText $ pack $ exportSecKey $ deriveSecKey $ pk
    , spacer
    , label "Make sure you backup your private key, don't give it to anyone!"
    , filler
    , mainButton "OK" done
    , filler
    ] `styleBasic` [ padding 20 ]
    where
      (Keys pk xo _ _) = fromJust $ model ^. backupKeys
