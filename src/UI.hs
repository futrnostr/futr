{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr
import           Data.DateTime
import           Data.Default
import           Data.List              (sortBy)
import qualified Data.Map               as Map
import           Data.Maybe             as Maybe
import           Data.Text              (Text, strip)
import qualified Data.Text              as T
import           Monomer
import qualified Monomer.Lens           as L
import           Monomer.Widgets.Single

import           AppTypes
import           Helpers
import           NostrFunctions
import           NostrTypes
import           UIHelpers
import qualified Widgets.EditProfile    as EditProfile
import qualified Widgets.ViewPosts      as ViewPosts
import qualified Widgets.ViewProfile    as ViewProfile

buildUI :: TChan ServerRequest -> AppWenv -> AppModel -> AppNode
buildUI channel wenv model = widgetTree
  where
    baseLayer = case model ^. currentView of
      PostsView ->
        vstack
          [ label "New Post"
          , spacer
          , vstack
              [ hstack
                  [ textArea newPostInput
                    `nodeKey` "newPost"
                    `styleBasic` [ height 50 ]
                  , filler
                  , button "Post" SendPost
                      `nodeEnabled` (strip (model ^. newPostInput) /= "")
                  ]
              ]
          , spacer
          , ViewPosts.viewPostsWidget
              wenv
              viewPostsModel
              (\re -> kind (fst re) == 1
                && NostrTypes.pubKey (fst re) `elem` (xo : userFollowing)
              )
              ViewPostDetails
              ViewProfile
          ]
          where
            (Keys _ xo _ _) = fromJust $ model ^. selectedKeys
            userFollowing = map mapProfileToXOnlyPubKey $
              Map.findWithDefault [] xo (model ^. following)
      PostDetailsView re ->
        viewPostUI wenv model re
      ProfileView xo ->
        ViewProfile.viewProfileWidget
          channel
          (fromJust $ model ^. selectedKeys)
          ViewPostDetails
          ViewProfile
          viewProfileModel
      EditProfileView ->
        EditProfile.editProfileWidget
          channel
          (fromJust $ model ^. selectedKeys)
          editProfileModel
    headerTree =
      hstack
        [ spacer, button "Back" Back `nodeVisible` (model ^. currentView /= PostsView)
        , filler
        , dropdown_
            selectedKeys
            (map (\k -> Just k) (model ^. keys))
            (currentKeysNode (model ^. receivedEvents))
            (currentKeysNode (model ^. receivedEvents))
            [ onChange KeysSelected ]
            `styleBasic` [ width 400 ]
        , spacer
        , button "Edit" EditProfile
        , spacer
        , button "Add" $ ShowDialog GenerateKeyPairDialog
        ] `styleBasic` [ paddingR 10 ]
    footerTree =
      vstack [hstack ([ filler ] ++ connections ++ [ spacer, addRelay ])]
        `styleBasic` [ padding 10 ]
      where
        connections =
          map
            (\r ->
              box_
                [ onClick $ ShowDialog $ RelayDialog r ]
                (tooltip (T.pack $ relayName r) (viewCircle r) `styleBasic`
                [ cursorIcon CursorHand ])
            )
            (sortBy sortPool (model ^. pool))
        addRelay =
          box_ [ alignTop, onClick $ ShowDialog NewRelayDialog ] $
          tooltip (T.pack $ "Add new relayy") $
          addIcon
        addIcon =
          icon IconPlus `styleBasic`
          [width 16, height 16, fgColor black, cursorHand]
    followingLayer = case model ^. selectedKeys of
      Nothing ->
        vstack []
      Just (Keys _ xo _ _)  ->
        vstack $
          case Map.lookup xo $ model ^. AppTypes.following of
            Just [] ->
              [ label "You don't follow anyone" ]
            Just ps ->
              map (\(Profile xo r pd) ->
                box_
                  [ onClick (ViewProfile xo), alignLeft ]
                  (profileBox xo $ pdName pd)
                    `styleBasic` [ cursorHand ]
              )
              ps
            Nothing ->
              [ label "You don't follow anyone" ]
      where
        keys = fromJust $ model ^. selectedKeys
    widgetTree =
      zstack
        [ vstack
            [ spacer
            , headerTree
            , spacer
            , hstack
                [ baseLayer `styleBasic` [ padding 10 ]
                , vstack
                    [ label "Following" `styleBasic` [ paddingB 10 ]
                    , spacer
                    , hstack
                        [ textField searchInput
                            `nodeKey` "searchInput"
                        , spacer
                        , button "Search" (SearchProfile (model ^. searchInput))
                            `nodeEnabled` isValidPubKey
                        ]
                    , spacer
                    , scroll_ [ scrollOverlay ] followingLayer
                    ]
                    `styleBasic` [ padding 10, width 200, borderL 1 sep ]
                ]
            , filler
            , footerTree
            ]
        , box_ [ alignCenter, alignMiddle ] (dialogLayer model)
          `nodeVisible` (model ^. dialog /= Nothing)
          `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
        ]
      where
        sep = rgbaHex "#A9A9A9" 0.75
        isValidPubKey = isJust $ maybe Nothing xOnlyPubKey $ decodeHex $ view searchInput model

dialogLayer :: AppModel -> AppNode
dialogLayer model =
  case md of
    Nothing ->
      vstack []
    Just d ->
      vstack
        [ hstack
            [ filler
            , box_ [alignTop, onClick CloseDialog] closeIcon
              `nodeEnabled` (d /= ErrorReadingKeysFileDialog && ks /= Nothing)
            ]
        , spacer
        , case d of
            GenerateKeyPairDialog -> generateOrImportKeyPairStack model
            ErrorReadingKeysFileDialog -> errorReadingKeysFileStack
            RelayDialog r -> viewRelayDialog r model
            NewRelayDialog -> viewNewRelayDialog model
        ] `styleBasic`
      [ width 500, height 400, padding 10, radius 10, bgColor darkGray ]
  where
    ks = model ^. selectedKeys
    md = model ^. dialog
    closeIcon = icon IconClose
      `styleBasic` [ width 16, height 16, fgColor black, cursorHand ]

currentKeysNode :: [ReceivedEvent] -> Maybe Keys -> AppNode
currentKeysNode res mks = case mks of
    Just (Keys _ xo _ n) ->
      case n of
        Just n' ->
          label n'
        Nothing ->
          label $ T.pack $ exportXOnlyPubKey xo
    _ ->
      label ""

viewPostUI :: AppWenv -> AppModel -> ReceivedEvent -> AppNode
viewPostUI wenv model re = widgetTree
  where
    event = fst re
    rs = snd re
    xo = NostrTypes.pubKey event
    postInfo =
      vstack
        [ hstack
            [ box_
                [ onClick (ViewProfile xo) ]
                (profileBox
                  xo
                  (profileName (model ^. profiles) xo)
                )
                `styleBasic` [ cursorHand ]
            , spacer
            , selectableText $ content event
            ]
        ] `styleBasic` [ paddingT 10 ]
    seenOnTree =
      vstack $
        map (\r -> label $ T.pack $ relayName r) (sortBy sortPool rs)
    widgetTree =
      vstack
        [ label "Post Details"
        , spacer
        , hstack
            [ textArea newPostInput
                `nodeKey` "replyPost"
                `styleBasic` [ height 50 ]
            , filler
            , button "Reply" (ReplyToPost event)
                `nodeEnabled` (strip (model ^. newPostInput) /= "")
            ]
        , spacer
        , scroll_ [ scrollOverlay ] $ postInfo `styleBasic` [ textTop ]
        , filler
        , spacer
        , label "Seen on"
        , spacer
        , seenOnTree
        ]

viewRelayDialog :: Relay -> AppModel -> AppNode
viewRelayDialog r model =
  vstack
    [ label (T.pack $ relayName r)
        `styleBasic` [ textSize 22 ]
    , filler
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
    , spacer
    , hstack
        [ label "Connection", spacer, button connStatus connButton ]
    , spacer
    , button "Save" $ UpdateRelay r
    ] `styleBasic`
  [padding 10]
  where
    connStatus =
      if connected r
        then "Disconnect"
        else "Connect"
    connButton =
      if connected r
        then DisconnectRelay r
        else ConnectRelay r

viewNewRelayDialog :: AppModel -> AppNode
viewNewRelayDialog model =
  vstack
    [ hstack
        [ label "Host"
        , spacer
        , textField (relayModel . relayHostInput) `nodeKey` "relayHostInput"
        ]
    , spacer
    , hstack
        [ label "Port"
        , spacer
        , numericField (relayModel . relayPortInput) `nodeKey` "relayPortInput"
        ]
    , spacer
    , hstack
        [ label "Secure connection"
        , spacer
        , checkbox (relayModel . relaySecureInput) `nodeKey` "relaySecureCheckbox"
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
    , spacer
    , button "Add relay" AddRelay
    ]
      `styleBasic` [ padding 10 ]

errorReadingKeysFileStack :: AppNode
errorReadingKeysFileStack =
  vstack
    [ label "ERROR: Keys file could not be read." ]

generateOrImportKeyPairStack :: AppModel -> AppNode
generateOrImportKeyPairStack model =
  vstack
    (title ++
    [ hstack
        [ label "Generate new key pair"
        , spacer
        , button "Generate" GenerateKeyPair
        ]
    , spacer
    , label "or import an existing private key"
    , spacer
    , hstack
        [ textField mySecKeyInput `nodeKey` "importmyprivatekey"
        , spacer
        , button "Import" ImportSecKey `nodeEnabled` isValidPrivateKey
        ]
    ]) `styleBasic` [ padding 10 ]
  where
    isValidPrivateKey =
      isJust $ maybe Nothing secKey $ decodeHex $ view mySecKeyInput model
    title = case model ^. selectedKeys of
      Nothing ->
        [ label "Welcome to nostr"
        , spacer
        , label "To get started, you need a valid key pair first"
        , spacer
        ]
      _ ->
        []

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
