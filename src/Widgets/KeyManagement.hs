{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.KeyManagement where

import Control.Concurrent.MVar
import Control.Monad.STM            (atomically)
import Control.Concurrent.STM.TChan
import Control.Lens
import Crypto.Schnorr
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.Map                     (Map)
import Data.Maybe
import Data.Text                    (Text, pack)
import Monomer

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map             as Map

import Helpers
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response
import UIHelpers
import Widgets.BackupKeys
import Widgets.ProfileImage

type KeyManagementWenv = WidgetEnv KeyManagementModel KeyManagementEvent

type KeyManagementNode = WidgetNode KeyManagementModel KeyManagementEvent

data KeyManagementModel = KeyManagementModel
  { _keyList         :: [Keys]
  , _backupKeysModel :: BackupKeysModel
  , _metadatas       :: Map XOnlyPubKey Profile
  } deriving (Eq, Show)

instance Default KeyManagementModel where
  def = KeyManagementModel [] def Map.empty

data KeyManagementEvent
  = GoSetup
  | BackToHome
  | DeleteKeys Keys
  | MarkAsMainKeys Keys
  | BackupKeys Keys
  | BackupDone
  deriving (Eq, Show)

makeLenses 'KeyManagementModel

keyManagementWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => ep
  -> ep
  -> ([Keys] -> ep)
  -> ALens' sp KeyManagementModel
  -> WidgetNode sp ep
keyManagementWidget goSetup goHome reportKeys model =
  composite
    "KeyManagementWidget"
    model
    viewKeyManagement
    (handleKeyManagementEvent goSetup goHome reportKeys)

handleKeyManagementEvent
  :: (WidgetEvent ep)
  => ep
  -> ep
  -> ([Keys] -> ep)
  -> KeyManagementWenv
  -> KeyManagementNode
  -> KeyManagementModel
  -> KeyManagementEvent
  -> [EventResponse KeyManagementModel KeyManagementEvent sp ep]
handleKeyManagementEvent goSetup goHome reportKeys env node model evt = case evt of
  GoSetup ->
    [ Report goSetup ]
  BackToHome ->
    [ Report goHome]
  DeleteKeys keys ->
    [ Model $ model & keyList .~ newKeyList'
    , Report $ reportKeys newKeyList'
    ]
    where
      newKeyList = filter (\k -> k /= keys) (model ^. keyList)
      newKeyList' = case length newKeyList of
        0 -> newKeyList
        1 -> map (\(Keys pk xo _ name) -> Keys pk xo True name) newKeyList
        _ -> do
          let firstKeys = head newKeyList
          let (Keys pk xo _ name) = firstKeys
          let mainKeys = Keys pk xo True name
          mainKeys : (tail newKeyList)

  MarkAsMainKeys (Keys kp xo active name) ->
    [ Model $ model & keyList .~ keys' : dk
    , Report $ reportKeys $ keys' : dk
    ]
    where
      keys' = Keys kp xo True name
      dk = disableKeys $ filter (\(Keys _ xo' _ _) -> xo' /= xo) $ model ^. keyList
  BackupKeys keys ->
    [ Model $ model & backupKeysModel . backupKeys .~ keys ]
  BackupDone ->
    [ Model $ model & backupKeysModel . backupKeys .~ initialKeys ]

viewKeyManagement :: KeyManagementWenv -> KeyManagementModel -> KeyManagementNode
viewKeyManagement wenv model =
  if (model ^. backupKeysModel . backupKeys) == initialKeys
    then keyManagementView
    else backupKeysWidget BackupDone (backupKeysModel)
  where
  {-
  myProfileImage = case model ^. currentImage of
    "" ->
      profileImage_ Nothing xo [ fitEither ] `styleBasic` [ width 300, height 300 ]
    pi ->
      profileImage_ (Just $ model ^. currentImage) xo [ fitEither ] `styleBasic` [ width 300, height 300 ]
  info = case model ^. currentImage of
    "" ->
      label "Robots lovingly delivered by Robohash.org" `styleBasic` [ textSize 8 ]
    _ ->
      hstack []
  -}
  keysRow idx (Keys pk xo active name) = box $
    hstack
      [ hstack
          [ spacer
          , profileImage Nothing xo `styleBasic` [ width 40, height 40 ]
          , spacer
          , label (fromMaybe "" name) `styleBasic` [ width 200 ]
          , filler
          , label $ middleXOnlyPubKey xo
          , spacer
          ]
          `styleBasic`
            [ border 1 $ rgbHex "#bae3ff"
            , radius 4
            , bgColor $ rgbHex "#7e7e7e"
            ]
      , filler
      , vstack [ filler, button "Set Active" (MarkAsMainKeys (Keys pk xo active name)), filler ] `nodeVisible` (active == False)
      , spacer
      , vstack [ filler, button "Backup" (BackupKeys (Keys pk xo active name)), filler ]
      , spacer
      , vstack [ filler, button "Delete" (DeleteKeys (Keys pk xo active name)), filler ]
      , spacer
      ] `styleBasic` [ paddingB 20, height 80 ]
  keyManagementView = vstack
    [ hstack [ button "Back" BackToHome, filler, bigLabel "Key Management", filler ]
    , spacer
    , hstack [ filler, button "New Account" GoSetup ]
    , spacer
    , vscroll_ [ scrollOverlay ] keys `styleBasic` [ paddingT 20 ]
    ] `styleBasic` [ padding 10 ]
    where
      keys = vstack keysRows
      keysFade idx k = animRow
        where
          Keys _ xo active name = k
          item = keysRow idx k
          animRow =
            animFadeOut_ [] item `nodeKey` (pack $ exportXOnlyPubKey xo)
      keysRows = zipWith keysFade [ 0 .. ] (model ^. keyList)
