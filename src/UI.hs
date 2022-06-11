{-# LANGUAGE OverloadedStrings #-}

module UI where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Crypto.Schnorr (exportXOnlyPubKey)
import Data.Default
import Data.List (sort)
import Data.Maybe
import Data.Text (Text)
import Monomer

import qualified Data.Map as Map
import qualified Monomer.Lens as L
import qualified Data.Text as T

import AppTypes
import Nostr.Keys
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import UIHelpers
import Widgets.ProfileImage

import qualified Widgets.BackupKeys as BackupKeys
import qualified Widgets.EditProfile as EditProfile
import qualified Widgets.Home as Home
import qualified Widgets.KeyManagement as KeyManagement
import qualified Widgets.RelayManagement as RelayManagement
import qualified Widgets.Setup as Setup

buildUI :: TChan Request -> MVar RelayPool -> AppWenv -> AppModel -> AppNode
buildUI channel poolMVar wenv model = widgetTree
  where
    Keys _ xo _ name = model ^. selectedKeys
    myProfileImage = case Map.lookup xo (model ^. profiles) of
      Nothing ->
        profileImage_ Nothing xo [ fitEither, alignCenter, alignMiddle ]
      Just ((Profile _ _ _ picture), _) ->
        profileImage_ picture xo [ fitEither, alignCenter, alignMiddle ]
    baseLayer = case model ^. currentView of
      HomeView ->
        Home.homeWidget channel homeModel
      SetupView ->
        Setup.setupWidget channel poolMVar NewKeysCreated setupModel
      BackupKeysView ->
        BackupKeys.backupKeysWidget KeysBackupDone backupKeysModel
      EditProfileView ->
        EditProfile.editProfileWidget
          channel
          (model ^. selectedKeys)
          ProfileUpdated
          GoHome
          editProfileModel
      KeyManagementView ->
        KeyManagement.keyManagementWidget
          GoSetup
          GoHome
          KeysUpdated
          keyMgmtModel
      RelayManagementView ->
        RelayManagement.relayManagementWidget
          poolMVar
          GoHome
          ConnectRelay
          DisconnectRelay
          RelaysUpdated
          relayMgmtModel
    imageButtonStyling =
      [ cursorHand
      , border 1 $ rgbHex "#bae3ff"
      , bgColor $ rgbHex "#7e7e7e"
      , height 40
      , padding 3
      , radius 4
      , width 60
      ]
    headerTree =
      hstack
        [ bigLabel "FuTr - nostr client" `styleBasic` [ padding 10 ]
        , filler
        , hstack
            [ box_
                [ onClick GoRelayManagement ] $
                tooltip "Relay Management" $ image_ "assets/icons/relay-icon.png" [ fitNone, alignCenter, alignMiddle ]
                `styleBasic` imageButtonStyling
            , spacer
            , box_
                [ onClick GoKeyManagement ] $
                tooltip "Key Management" $ image_ "assets/icons/keys-icon.png" [ fitNone, alignCenter, alignMiddle ]
                `styleBasic` imageButtonStyling
            , spacer
            , box_
                [ onClick EditProfile ] $
                tooltip "Edit Account" $ myProfileImage
                `styleBasic` imageButtonStyling
            ] `nodeVisible` (model ^. currentView == HomeView)
        ] `styleBasic` [ paddingR 10, paddingT 10 ]
    footerTree =
      hstack [ currentKeyInfo, filler, hstack connections ]
        `styleBasic` [ padding 10 ]
      where
        connections =
          map
            (\r ->
              box $
                tooltip (relayName r) (viewCircle r)
                `styleBasic` [ cursorIcon CursorHand ]
            )
            (sort $ model ^. relays)
        currentKeyInfo = selectableText accountData `styleBasic` [ textSize 12 ]
          where
            accountData = case name of
              Just n ->
                n `T.append` " - PubKey: " `T.append` (T.pack $ exportXOnlyPubKey xo)
              Nothing ->
                "PubKey: " `T.append` (T.pack $ exportXOnlyPubKey xo)
    widgetTree =
      zstack
        [ vstack
            [ headerTree
            , spacer
            , baseLayer `styleBasic` [ padding 10 ]
            , filler
            , footerTree
            ]
        , box_ [ alignCenter, alignMiddle ] (errorLayer $ model ^. errorMsg)
          `nodeVisible` (model ^. errorMsg /= Nothing)
          `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
        ]

errorLayer :: Maybe Text -> AppNode
errorLayer errorMsg' =
  vstack
    [ hstack
        [ filler
        , bigLabel "ERROR"
        , filler
        ]
    , spacer
    , label_
        (fromMaybe "" errorMsg')
        [ multiline
        , maxLines 4
        ]
    ]
    `styleBasic`
      [ width 500
      , height 200
      , padding 10
      , radius 10
      , bgColor darkGray
      ]
