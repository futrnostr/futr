{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.RelayManagement where

import Control.Concurrent.MVar
import Control.Exception
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

data RelayModel =
  RelayModel
    { _relayURI           :: Text
    , _relayReadableInput :: Bool
    , _relayWritableInput :: Bool
    , _isInvalidInput     :: Bool
    }
  deriving (Eq, Show)

instance Default RelayModel where
  def = RelayModel "wss://" True True False

data RelayManagementModel = RelayManagementModel
  { _rmRelays        :: [Relay]
  , _relayToRemove   :: Maybe Relay
  , _relayModel      :: RelayModel
  , _displayAddRelay :: Bool
  } deriving (Eq, Show)

instance Default RelayManagementModel where
  def = RelayManagementModel [] Nothing def False

data RelayManagementEvent
  = BackToHome
  -- add
  | DisplayAddRelay
  | CancelAddRelay
  | ValidateAndAddRelay
  | InvalidRelayURI
  | AddRelay Relay
  -- remove
  | RemoveRelay Relay
  | ConfirmRemoveRelay
  | CancelRemoveRelay
  -- connection
  | SendConnectRelay Relay
  | SendDisconnectRelay Relay
  | SendRelaysUpdated [Relay]
  deriving Show

makeLenses 'RelayModel
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
  ValidateAndAddRelay ->
    [ Task $ validateAndAddRelay (model ^. relayModel) ]
  InvalidRelayURI ->
    [ Model $ model & relayModel . isInvalidInput .~ True ]
  DisplayAddRelay ->
    [ Model $ model & displayAddRelay .~ True ]
  CancelAddRelay ->
    [ Model $ model & displayAddRelay .~ False ]
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

doAddRelay :: MVar RelayPool -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
doAddRelay poolMVar relay sendMsg = do
  newRelays <- addRelay poolMVar relay
  putStrLn $ show newRelays
  sendMsg $ SendRelaysUpdated newRelays
  sendMsg $ SendConnectRelay relay

doRemoveRelay :: MVar RelayPool -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
doRemoveRelay poolMVar relay sendMsg = do
  sendMsg $ SendDisconnectRelay relay
  newRelays <- removeRelay poolMVar relay
  sendMsg $ SendRelaysUpdated newRelays

validateAndAddRelay :: RelayModel -> IO RelayManagementEvent
validateAndAddRelay model = do
  uriE <- try $ URI.mkURI $ model ^. relayURI :: IO (Either URI.ParseException URI.URI)
  case uriE of
    Right uri -> do
      return $ if isValidRelayURI uri
        then do
          let info = RelayInfo (model ^. relayReadableInput) (model ^. relayWritableInput)
          AddRelay (Relay uri info False)
        else
          InvalidRelayURI
    Left err ->
      return $ InvalidRelayURI

isValidRelayURI :: URI.URI -> Bool
isValidRelayURI u =
  case u ^. uriScheme of
    (Just s) -> if (URI.unRText s) `elem` ["ws", "wss"]
      then validateAuthority u
      else False
    _        -> False

validateAuthority :: URI.URI -> Bool
validateAuthority u = host /= "" where
  host = case u ^. uriAuthority of
    Right (URI.Authority _ host' _) -> URI.unRText host'
    _ -> ""

viewRelayManagement :: RelayManagementWenv -> RelayManagementModel -> RelayManagementNode
viewRelayManagement wenv model = relaysView where
  relaysRow idx relay = box $
    hstack
      [ hstack
          [ spacer
          , label $ relayName relay
          , filler
          , separatorLine
          , spacer
          , spacer
          , vstack
              [ filler
              , label rLabel
              , spacer
              , label wLabel
              , filler
              ]
          , spacer
          , spacer
          , separatorLine
          , spacer
          , spacer
          , vstack
              [ filler
              , box_ [ onClick doConnect ] $ tooltip iconLabel (viewCircle relay)
                  `styleBasic` [ cursorIcon CursorHand ]
              , filler
              ] `styleBasic` [ paddingT 5 ]
          , spacer
          , vstack
              [ filler
              , box_ [ onClick (RemoveRelay relay) ]
                  $ tooltip "Remove Relay"
                  $ icon_ IconClose [ width 2 ]
                      & L.info . L.style .~ collectTheme wenv L.dialogCloseIconStyle
              , filler
              ]
          , spacer
          ]
          `styleBasic`
            [ border 1 $ rgbHex "#bae3ff"
            , radius 4
            , padding 10
            ]
      ] `styleBasic` [ paddingB 20, height 80 ]
      where
        iconLabel = if connected relay then "Disconnect" else "Connected"
        doConnect = if connected relay then SendDisconnectRelay relay else SendConnectRelay relay
        doConnectLabel = if connected relay then "Disconnect" else "Connect"
        rLabel = if readable $ info relay then "readable" else "not readable"
        wLabel = if writable $ info relay then "writable" else "not writable"
  recommendedRelays = vstack
    [ label "Recommended Relays"
    , spacer
    , spacer
    , label "coming soon"
    ]
  relaysView = vstack
    [ hstack [ button "Back" BackToHome, filler, bigLabel "Relay Management", filler ]
    , spacer
    , hstack [ filler, mainButton "Add Relay" DisplayAddRelay ]
    , spacer
    , spacer
    , zstack
        [ hstack
            [ vscroll_ [ scrollOverlay ] relays `styleBasic` [ paddingT 20, width 600 ]
            , spacer
            , separatorLine
            , spacer
            , recommendedRelays
            ]
        , confirmMsg_
            "Are you sure you want to remove this relay?"
            ConfirmRemoveRelay
            CancelRemoveRelay
            [ acceptCaption "Remove Relay" ]
            `nodeVisible` (model ^. relayToRemove /= Nothing)
            `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
        , box_ [ alignCenter, alignMiddle ] (newRelayDialog model)
            `nodeVisible` (model ^. displayAddRelay == True)
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

newRelayDialog :: RelayManagementModel -> RelayManagementNode
newRelayDialog model = vstack
  [ hstack
      [ bigLabel "Add New Relay"
      , filler
      , box_ [alignTop, onClick CancelAddRelay ] closeIcon
      ]
  , spacer
  , vstack
      [ hstack
          [ label "URI"
          , spacer
          , textField (relayModel . relayURI) `nodeKey` "relayURI"
          ]
      , spacer
      , hstack
          [ label "Readable"
          , spacer
          , checkbox (relayModel . relayReadableInput) `nodeKey` "relayReadableCheckbox"
          ]
      , spacer
      , hstack
          [ label "Writable"
          , spacer
          , checkbox (relayModel . relayWritableInput) `nodeKey` "relayWriteableCheckbox"
          ]
      , filler `nodeVisible` (model ^. relayModel . isInvalidInput)
      , label "Invalid Relay URI given" `nodeVisible` (model ^. relayModel . isInvalidInput)
      , filler
      , button "Add relay" ValidateAndAddRelay
      ] `styleBasic` [ padding 10 ]
  ] `styleBasic` [ width 500, height 250, padding 10, radius 10, bgColor darkGray ]
  where
    closeIcon = icon IconClose
      `styleBasic` [ width 16, height 16, fgColor black, cursorHand ]
