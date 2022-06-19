{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.KeyManagement where

import Control.Lens
import Crypto.Schnorr
import Data.DateTime
import Data.Default
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, pack)
import Monomer

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Monomer.Lens as L

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
import Widgets.ProfileImage

import qualified Widgets.BackupKeys as BackupKeys

type KeyManagementWenv = WidgetEnv KeyManagementModel KeyManagementEvent

type KeyManagementNode = WidgetNode KeyManagementModel KeyManagementEvent

data KeyManagementModel = KeyManagementModel
  { _keyList         :: [Keys]
  , _backupKeysModel :: BackupKeys.BackupKeysModel
  , _kmProfiles       :: Map XOnlyPubKey (Profile, DateTime)
  , _keysToDelete      :: Maybe Keys
  } deriving (Eq, Show)

instance Default KeyManagementModel where
  def = KeyManagementModel [] def Map.empty Nothing

data KeyManagementEvent
  = GoSetup
  | BackToHome
  | DeleteKeys Keys
  | ConfirmDeleteKeys
  | CancelDeleteKeys
  | MarkAsMainKeys Keys
  | BackupKeys Keys
  | BackupDone
  deriving Show

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
    buildUI
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
    [ Model $ model & keysToDelete .~ Just keys ]
  ConfirmDeleteKeys ->
    [ Model $ model
        & keyList .~ newKeyList'
        & keysToDelete .~ Nothing
    , Report $ reportKeys newKeyList'
    ]
    where
      newKeyList = filter (\k -> k /= (fromJust $ model ^. keysToDelete)) (model ^. keyList)
      newKeyList' = case length newKeyList of
        0 -> newKeyList
        1 -> map (\(Keys pk xo _ name) -> Keys pk xo True name) newKeyList
        _ -> do
          let (Keys pk xo _ name) = head newKeyList
          Keys pk xo True name : (tail newKeyList)
  CancelDeleteKeys ->
    [ Model $ model & keysToDelete .~ Nothing ]
  MarkAsMainKeys (Keys kp xo active name) ->
    [ Model $ model & keyList .~ keys' : dk
    , Report $ reportKeys $ keys' : dk
    ]
    where
      keys' = Keys kp xo True name
      dk = disableKeys $ filter (\(Keys _ xo' _ _) -> xo' /= xo) $ model ^. keyList
  BackupKeys keys ->
    [ Model $ model & backupKeysModel . BackupKeys.backupKeys .~ keys ]
  BackupDone ->
    [ Model $ model & backupKeysModel . BackupKeys.backupKeys .~ initialKeys ]

buildUI :: KeyManagementWenv -> KeyManagementModel -> KeyManagementNode
buildUI wenv model =
  if (model ^. backupKeysModel . BackupKeys.backupKeys) == initialKeys
    then keyManagementView
    else BackupKeys.backupKeysWidget BackupDone (backupKeysModel)
  where
  pictureUrl xo = case Map.lookup xo (model ^. kmProfiles) of
    Just ((Profile _ _ _ picture), _) ->
      picture
    Nothing ->
      Nothing

  keysRow idx (Keys pk xo active name) = box $
    hstack
      [ hstack
          [ spacer
          , vstack
              [ filler
              , profileImage (pictureUrl xo) xo Small
              , filler
              ]
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
    , zstack
        [ vscroll_ [ scrollOverlay ] keys `styleBasic` [ paddingT 20 ]
        , confirmMsg_
            "Are you sure you want to delete those keys? If you don't have a backup there is no way to get it back."
            ConfirmDeleteKeys
            CancelDeleteKeys
            [ acceptCaption "Delete keys" ]
            `nodeVisible` (model ^. keysToDelete /= Nothing)
            `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
        ]
    ]
    where
      keys = vstack keysRows
      keysFade idx k = animRow
        where
          Keys _ xo active name = k
          item = keysRow idx k
          animRow =
            animFadeOut_ [] item `nodeKey` (pack $ exportXOnlyPubKey xo)
      keysRows = zipWith keysFade [ 0 .. ] (model ^. keyList)
