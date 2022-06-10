{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr         (exportXOnlyPubKey)
import           Data.Default
import           Data.List              (sort)
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Monomer
import qualified Monomer.Lens           as L
import           Monomer.Widgets.Single

import           AppTypes
import           Nostr.Keys
import           Nostr.Relay
import           Nostr.RelayPool
import           Nostr.Request
import           UIHelpers
import           Widgets.ProfileImage

import qualified Widgets.BackupKeys  as BackupKeys
import qualified Widgets.EditProfile as EditProfile
import qualified Widgets.Home        as Home
import qualified Widgets.Setup       as Setup

buildUI :: TChan Request -> MVar RelayPool -> AppWenv -> AppModel -> AppNode
buildUI channel poolMVar wenv model = widgetTree
  where
    Keys _ xo _ name = model ^. selectedKeys
    myProfileImage = case model ^. homeModel . Home.profileImage of
      "" ->
        profileImage_ Nothing xo [ fitEither ] `styleBasic` [ width 40, height 40 ]
      pi ->
        profileImage_ (Just pi) xo [ fitEither ] `styleBasic` [ width 40, height 40 ]
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
    imageButtonStyling =
      [ padding 3
      , width 60
      , height 40
      , cursorHand
      , border 1 $ rgbHex "#bae3ff"
      , radius 4
      , bgColor $ rgbHex "#7e7e7e"
      ]
    headerTree =
      hstack
        [ bigLabel "FuTr - nostr client" `styleBasic` [ padding 10 ]
        , filler
        , hstack
            [ box_
                [ onClick NoOp ]
                (tooltip "Key Management" $ image_ "assets/icons/keys-icon.png" [ fitNone, alignCenter, alignMiddle ]
                )
                `styleBasic` imageButtonStyling
            , spacer
            , box_
                [ onClick EditProfile ]
                (tooltip "My Profile" myProfileImage)
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
              box
                (tooltip (relayName r) (viewCircle r) `styleBasic`
                [ cursorIcon CursorHand ])
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

viewCircle :: Relay -> WidgetNode AppModel AppEvent
viewCircle r = defaultWidgetNode "circlesGrid" widget
  where
    widget =
      createSingle () def {singleGetSizeReq = getSizeReq, singleRender = render}
    getSizeReq wenv node = (fixedSize 20, fixedSize 20)
    render wenv node renderer = do
      drawCircle renderer vp r
      where
        style = currentStyle wenv node
        vp = getContentArea node style

drawCircle :: Renderer -> Rect -> Relay -> IO ()
drawCircle renderer vp r = do
  let offsetX = -3
  let offsetY = 0
  let color =
        if connected r
          then paleGreen
          else orange
  let colorFill = color & L.a .~ 0.3
  beginPath renderer
  setStrokeWidth renderer 2
  setStrokeColor renderer color
  setFillColor renderer colorFill
  renderEllipse renderer (rect offsetX offsetY)
  fill renderer
  stroke renderer
  where
    size = 15
    rect ox oy = Rect rx ry size size
      where
        rx = vp ^. L.x + vp ^. L.w + ox - size
        ry = vp ^. L.y + oy
