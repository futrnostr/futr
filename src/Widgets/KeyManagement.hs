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
import Data.Text                    (Text, pack, unpack)
import Monomer

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map             as Map

import Helpers
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response
import UIHelpers
import Widgets.ProfileImage

type KeyManagementWenv = WidgetEnv KeyManagementModel KeyManagementEvent

type KeyManagementNode = WidgetNode KeyManagementModel KeyManagementEvent

data KeyManagementModel = KeyManagementModel
  { _keyList :: [Keys]
  , _backup  :: Maybe Keys
  } deriving (Eq, Show)

instance Default KeyManagementModel where
  def = KeyManagementModel [] Nothing

data KeyManagementEvent
  = GoSetup
  | BackToHome
  | DeleteKeys Keys
  | MarkAsMainKey Keys
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
    [ Model $ model & keyList .~ newKeyList
    , Report $ reportKeys newKeyList
    ]
    where
      newKeyList = filter (\k -> k /= keys) (model ^. keyList)
  MarkAsMainKey keys ->
    [ Model $ model & keyList .~ keys : dk
    , Report $ reportKeys $ keys : dk
    ]
    where
      dk = disableKeys $ filter (\keys' -> keys' /= keys) $ model ^. keyList
  BackupKeys keys ->
    [ Model $ model & backup .~ (Just keys) ]
  BackupDone ->
    [ Model $ model & backup .~ Nothing ]

viewKeyManagement :: KeyManagementWenv -> KeyManagementModel -> KeyManagementNode
viewKeyManagement wenv model = keyManagementView where
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
  keysRow idx (Keys pk xo active name) = hstack [ label $ pack $ exportXOnlyPubKey xo ]
  keyManagementView = vstack
    [ hstack [ button "Back" BackToHome, filler, bigLabel "Key Management", filler ]
    , filler
    , vscroll_ [ scrollOverlay ] keys
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

