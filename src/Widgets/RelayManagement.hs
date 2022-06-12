{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Widgets.RelayManagement where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Lens
import Control.Monad.STM (atomically)
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
import Nostr.RelayConnection
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response
import UIHelpers

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
  | ConnectRelay Relay
  | DisconnectRelay Relay
  | RelaysUpdated [Relay]
  deriving Show

makeLenses 'RelayModel
makeLenses 'RelayManagementModel

relayManagementWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> MVar RelayPool
  -> ep
  -> ([Relay] -> ep)
  -> ALens' sp RelayManagementModel
  -> WidgetNode sp ep
relayManagementWidget channel poolMVar goHome relaysUpdated model =
  composite
    "RelayManagementWidget"
    model
    buildUI
    (handleRelayManagementEvent channel poolMVar goHome relaysUpdated)

handleRelayManagementEvent
  :: (WidgetEvent ep)
  => TChan Request
  -> MVar RelayPool
  -> ep
  -> ([Relay] -> ep)
  -> RelayManagementWenv
  -> RelayManagementNode
  -> RelayManagementModel
  -> RelayManagementEvent
  -> [EventResponse RelayManagementModel RelayManagementEvent sp ep]
handleRelayManagementEvent channel poolMVar goHome relaysUpdated env node model evt =
  case evt of
    BackToHome ->
      [ Model $ model
          & relayModel .~ def
          & displayAddRelay .~ False
      , Report goHome
      ]
    ValidateAndAddRelay ->
      [ Task $ validateAndAddRelay (model ^. relayModel) ]
    InvalidRelayURI ->
      [ Model $ model & relayModel . isInvalidInput .~ True ]
    DisplayAddRelay ->
      [ Model $ model & displayAddRelay .~ True ]
    CancelAddRelay ->
      [ Model $ model & displayAddRelay .~ False ]
    AddRelay relay ->
      [ Producer $ doAddRelay channel poolMVar relay
      , Model $ model
          & displayAddRelay .~ False
          & relayModel .~ def
      ]
    RemoveRelay relay ->
      [ Model $ model & relayToRemove .~ Just relay ]
    ConfirmRemoveRelay ->
      [ Producer $ doRemoveRelay channel poolMVar (fromJust $ model ^. relayToRemove)
      , Model $ model & relayToRemove .~ Nothing
      ]
    CancelRemoveRelay ->
      [ Model $ model & relayToRemove .~ Nothing ]
    ConnectRelay relay ->
      [ Producer $ connectRelay channel poolMVar relay ]
    DisconnectRelay relay ->
      [ Producer $ disconnectRelay channel relay ]
    RelaysUpdated relays ->
      [ Report $ relaysUpdated relays
      , Model $ model & rmRelays .~ relays
      ]

connectRelay :: TChan Request -> MVar RelayPool -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
connectRelay channel poolMVar relay sendMsg =
  connect channel poolMVar sendMsg RelaysUpdated relay

disconnectRelay :: TChan Request -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
disconnectRelay channel relay _ =
  atomically $ writeTChan channel $ Disconnect relay

doAddRelay :: TChan Request -> MVar RelayPool -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
doAddRelay channel poolMVar relay sendMsg = do
  newRelays <- addRelay poolMVar relay
  sendMsg $ RelaysUpdated newRelays
  connect channel poolMVar sendMsg RelaysUpdated relay

doRemoveRelay :: TChan Request -> MVar RelayPool -> Relay -> (RelayManagementEvent -> IO ()) -> IO ()
doRemoveRelay channel poolMVar relay sendMsg = do
  disconnectRelay channel relay sendMsg
  newRelays <- removeRelay poolMVar relay
  sendMsg $ RelaysUpdated newRelays

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

buildUI :: RelayManagementWenv -> RelayManagementModel -> RelayManagementNode
buildUI wenv model = relaysView where
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
        doConnect = if connected relay then DisconnectRelay relay else ConnectRelay relay
        doConnectLabel = if connected relay then "Disconnect" else "Connect"
        rLabel = if readable $ info relay then "readable" else "not readable"
        wLabel = if writable $ info relay then "writable" else "not writable"
  recommendedRelays = vstack
    [ label "Recommended Relays"
    , spacer
    , spacer
    , label "coming soon"
    ]
  relaysView =
    zstack
      [ vstack
          [ hstack [ button "Back" BackToHome, filler, bigLabel "Relay Management", filler ]
          , spacer
          , hstack [ filler, mainButton "Add Relay" DisplayAddRelay ]
          , spacer
          , spacer
          , hstack
              [ vscroll_ [ scrollOverlay ] relays `styleBasic` [ paddingT 20, width 600 ]
              , spacer
              , separatorLine
              , spacer
              , recommendedRelays
              ]
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
          `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
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
