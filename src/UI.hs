{-# LANGUAGE OverloadedStrings #-}

module UI where

import Debug.Trace

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Crypto.Schnorr (exportXOnlyPubKey)
import Data.Default
import Data.List (sort)
import Data.Maybe
import Data.Text (Text, strip)
import Monomer

import qualified Data.Map as Map
import qualified Monomer.Lens as L
import qualified Data.Text as T

import AppTypes
import Futr
import Nostr.Event
import Nostr.Keys
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import UIHelpers
import Widgets.ProfileImage

import qualified Widgets.BackupKeys as BackupKeys
import qualified Widgets.EditProfile as EditProfile
import qualified Widgets.KeyManagement as KeyManagement
import qualified Widgets.PostDetails as PostDetails
import qualified Widgets.RelayManagement as RelayManagement
import qualified Widgets.Setup as Setup
import qualified Widgets.ViewPosts as ViewPosts
import qualified Widgets.Profile as Profile

buildUI :: MVar RelayPool -> TChan Request -> AppWenv -> AppModel -> AppNode
buildUI pool request wenv model = widgetTree
  where
    isLoggedIn = isJust $ model ^. futr . selectedKeys
    baseLayer = case model ^. currentView of
      HomeView ->
        if model ^. waitingForConns
          then waitingForConnectionsTree
          else homeUI wenv model
      SetupView ->
        if model ^. waitingForConns
          then waitingForConnectionsTree
          else Setup.setupWidget pool request NewKeysCreated setupModel
      BackupKeysView ->
        BackupKeys.backupKeysWidget KeysBackupDone backupKeysModel
      EditProfileView ->
        EditProfile.editProfileWidget
          request
          (fromJust $ model ^. futr . selectedKeys)
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
          pool
          request
          GoHome
          AppTypes.RelaysUpdated
          relayMgmtModel
      PostDetailsView ->
        PostDetails.postDetailsWidget
          pool
          request
          GoHome
          ViewPostDetails
          ViewProfile
          ReplyToPost
          postDetailsModel
      ProfileView ->
        Profile.profileWidget
          pool
          request
          GoHome
          ViewPostDetails
          ViewProfile
          Follow
          Unfollow
          profileModel
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
        [ bigLabel "futr - nostr client" `styleBasic` [ padding 10 ]
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
            , case model ^. futr . selectedKeys of
                Nothing ->
                  tooltip "Edit Account" $ label "N/A" `styleBasic` imageButtonStyling
                Just (Keys _ xo _ _) ->
                  box_
                    [ onClick $ if model ^. waitingForConns then NoOp else EditProfile ] $
                    tooltip "Edit Account" $ profileImage pImage xo Small
                    `styleBasic` imageButtonStyling
                  where
                    pImage = Map.lookup xo (model ^. futr . profiles) >>= (\((Profile _ _ _ p), _) -> p)
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
            accountData = fromMaybe "Not logged in" $ do
              (Keys _ xo _ name) <- model ^. futr . selectedKeys
              case name of
                Just n ->
                  return $ n `T.append` " - PubKey: " `T.append` (T.pack $ exportXOnlyPubKey xo)
                Nothing ->
                  return $ "PubKey: " `T.append` (T.pack $ exportXOnlyPubKey xo)
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
    waitingForConnectionsTree =
      vstack
        [ filler
        , hstack
            [ filler
            , label "Waiting for relay connections to become available..."
            , filler
            ]
        , spacer
        , hstack
            [ filler
            , label "Check your internet connection or try to reconnect manually in the Relay Management"
            , filler
            ]
        , filler
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

homeUI :: AppWenv -> AppModel -> WidgetNode AppModel AppEvent
homeUI wenv model =
  vstack
    [ hstack
        [ label "New Post"
        , filler
        , textField searchInput `nodeKey` "searchInput" `styleBasic` [ width 250 ]
        , spacer
        , button "Search" Search
        ]
    , spacer
    , vstack
        [ hstack
            [ textArea (inputField)
              `nodeKey` "noteInput"
              `styleBasic` [ height 50 ]
            , filler
            , button "Post" (SendPost $ model ^. inputField)
              `nodeEnabled` ((strip $ model ^. inputField) /= "")
            ]
        ]
    , spacer
    , ViewPosts.viewPosts
        ViewPostDetails
        ViewProfile
        wenv
        (model ^. futr)
        (model ^. futr . events)
    ]
