{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.RelayManagement where

import Control.Concurrent.MVar
import Control.Lens
import Crypto.Schnorr
import Data.DateTime
import Data.Default
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, pack)
import Monomer
import Text.URI.Lens

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import qualified Monomer.Lens as L
import qualified Text.URI as URI

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

type RelayManagementWenv = WidgetEnv RelayManagementModel RelayManagementEvent

type RelayManagementNode = WidgetNode RelayManagementModel RelayManagementEvent

data RelayManagementModel = RelayManagementModel
  { _rmRelays      :: [Relay]
  , _relayToRemove :: Maybe Relay
  , _relayModel    :: RelayModel
  } deriving (Eq, Show)

instance Default RelayManagementModel where
  def = RelayManagementModel [] Nothing def

data RelayManagementEvent
  = BackToHome
  | ValidateRelay Relay
  | InvalidRelayURI
  | AddRelay Relay
  | RemoveRelay Relay
  | ConfirmRemoveRelay
  | CancelRemoveRelay
  | SendConnectRelay Relay
  | SendDisconnectRelay Relay
  | SendRelaysUpdated [Relay]
  deriving (Eq, Show)

makeLenses 'RelayManagementModel

relayManagementWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => MVar RelayPool
  -> ep
  -> (Relay -> ep)
  -> (Relay -> ep)
  -> ([Relay] -> ep)
  -> ALens' sp RelayManagementModel
  -> WidgetNode sp ep
relayManagementWidget poolMVar goHome connectRelay disconnectRelay relaysUpdated model =
  composite
    "RelayManagementWidget"
    model
    viewRelayManagement
    (handleRelayManagementEvent poolMVar goHome connectRelay disconnectRelay relaysUpdated)

handleRelayManagementEvent
  :: (WidgetEvent ep)
  => MVar RelayPool
  -> ep
  -> (Relay -> ep)
  -> (Relay -> ep)
  -> ([Relay] -> ep)
  -> RelayManagementWenv
  -> RelayManagementNode
  -> RelayManagementModel
  -> RelayManagementEvent
  -> [EventResponse RelayManagementModel RelayManagementEvent sp ep]
handleRelayManagementEvent poolMVar goHome connectRelay disconnectRelay relaysUpdated env node model evt = case evt of
  BackToHome ->
    [ Report goHome ]
  ValidateRelay relay ->
    [ Task $ validateRelay (model ^. relayModel) ]
  InvalidRelayURI ->
    [ Model $ model & relayModel . isInvalidInput .~ True ]
  AddRelay relay ->
    [ Producer $ doAddRelay poolMVar relay ]
  RemoveRelay relay ->
    [ Model $ model & relayToRemove .~ Just relay ]
  ConfirmRemoveRelay ->
    [ Producer $ doRemoveRelay poolMVar (fromJust $ model ^. relayToRemove)
    , Model $ model & relayToRemove .~ Nothing
    ]
  CancelRemoveRelay ->
    [ Model $ model & relayToRemove .~ Nothing ]
  SendConnectRelay relay ->
    [ Report $ connectRelay relay ]
  SendDisconnectRelay relay ->
    [ Report $ disconnectRelay relay ]
  SendRelaysUpdated relays ->
    [ Report $ relaysUpdated relays ]

viewRelayManagement :: RelayManagementWenv -> RelayManagementModel -> RelayManagementNode
viewRelayManagement wenv model = relaysView where
  relaysRow idx relay = box $
    hstack
      [ hstack
          [ spacer
          , label $ relayName relay
          , filler
          , vstack
              [ filler
              , separatorLine
              , label rLabel
              , spacer
              , label wLabel
              , separatorLine
              , filler
              ]
          , spacer
          , box $ tooltip iconLabel (viewCircle relay) `styleBasic` [ cursorIcon CursorHand ]
          , spacer
          ]
          `styleBasic`
            [ border 1 $ rgbHex "#bae3ff"
            , radius 4
            , bgColor $ rgbHex "#7e7e7e"
            ]
      , filler
      , vstack [ filler, button doConnectLabel doConnect, filler ]
      , spacer
      , vstack [ filler, button "Remove" (RemoveRelay relay), filler ]
      , spacer
      ] `styleBasic` [ paddingB 20, height 80 ]
      where
        iconLabel = if connected relay then "Connected" else "Disconnected"
        doConnect = if connected relay then SendDisconnectRelay relay else SendConnectRelay relay
        doConnectLabel = if connected relay then "Disconnect" else "Connect"
        rLabel = if readable $ info relay then "readable" else "not readable"
        wLabel = if writable $ info relay then "writable" else "not writable"
  relaysView = vstack
    [ hstack [ button "Back" BackToHome, filler, bigLabel "Relay Management", filler ]
    , spacer
    , hstack [ filler, button "Add Relay" BackToHome ]
    , spacer
    , zstack
        [ vscroll_ [ scrollOverlay ] relays `styleBasic` [ paddingT 20 ]
        , confirmMsg "Are you sure you want to remove this relay?" ConfirmRemoveRelay CancelRemoveRelay
          `nodeVisible` (model ^. relayToRemove /= Nothing)
          `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
        ]
    ]
    where
      relays = vstack relaysRows
      relaysFade idx r = animRow
        where
          item = relaysRow idx r
          animRow =
            animFadeOut_ [] item `nodeKey` (relayName r)
      relaysRows = zipWith relaysFade [ 0 .. ] (model ^. rmRelays)

doAddRelay :: MVar RelayPool -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
doAddRelay poolMVar relay sendMsg = do
  newRelays <- addRelay poolMVar relay
  sendMsg $ SendRelaysUpdated newRelays
  sendMsg $ SendConnectRelay relay

doRemoveRelay :: MVar RelayPool -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
doRemoveRelay poolMVar relay sendMsg = do
  sendMsg $ SendDisconnectRelay relay
  newRelays <- removeRelay poolMVar relay
  sendMsg $ SendRelaysUpdated newRelays

validateRelay :: RelayModel -> IO RelayManagementEvent
validateRelay model = do
  uri <- URI.mkURI $ model ^. relayURI
  return $ if isValidRelayURI uri
    then do
      let info = RelayInfo (model ^. relayReadableInput) (model ^. relayWritableInput)
      AddRelay (Relay uri info False)
    else
      InvalidRelayURI