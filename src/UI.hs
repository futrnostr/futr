{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr
import           Data.Default
import           Data.List              (sortBy)
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
import           Widgets.Profile

buildUI :: TChan ServerRequest -> AppWenv -> AppModel -> AppNode
buildUI channel wenv model = widgetTree
  where
    baseLayer = case model ^. currentView of
      PostsView ->
        viewPosts wenv model
      ProfileView ->
        profileWidget channel (fromJust $ model ^. selectedKeys) profileModel
      PostDetailsView re ->
        viewPostUI wenv model re
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
        , button "Edit" ViewProfile
        , spacer
        , button "Add" $ ShowDialog GenerateKeyPairDialog
        ] `styleBasic` [ paddingL 10, paddingR 10 ]
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
    widgetTree =
      zstack
        [ vstack
            [ spacer
            , headerTree
            , spacer
            , baseLayer
            , filler
            , footerTree
            ]
        , box_ [ alignCenter, alignMiddle ] (dialogLayer model)
          `nodeVisible` (model ^. dialog /= Nothing)
          `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
        ]

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
    Just (Keys _ xo _) ->
      label $ profileName res $ xo
    Nothing ->
      label ""

postRow :: AppWenv -> [ReceivedEvent] -> Int -> ReceivedEvent -> AppNode
postRow wenv res idx re = row
  where
    e = fst re
    rowSep = rgbaHex "#A9A9A9" 0.75
    rowBg = wenv ^. L.theme . L.userColorMap . at "rowBg" . non def
    postInfo =
      hstack
        [ vstack
            [ selectableText $ profileName res $ NostrTypes.pubKey e
            , selectableText
              $ shortXOnlyPubKey
              $ NostrTypes.pubKey e
            ]
        , spacer
        , selectableText $ content e
        ]
    row =
      hstack
        [ postInfo
        , spacer
        , button "Details" (ViewPostDetails re)
        ] `styleBasic` [ paddingT 10, paddingB 10, borderB 1 rowSep ]

viewPosts :: AppWenv -> AppModel -> AppNode
viewPosts wenv model = widgetTree
  where
    posts = vstack postRows
      where
        -- display only kind 1 events
        orderedPosts = filter (\re -> kind (fst re) == 1) (model ^. receivedEvents)
        postFade idx e = animRow
          where
            action = ViewPostDetails e
            item = postRow wenv (model ^. receivedEvents) idx e
            animRow =
              animFadeOut_ [onFinished action] item `nodeKey` (content $ fst e)
        postRows = zipWith postFade [0 ..] orderedPosts
    widgetTree =
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
        , scroll_ [scrollOverlay] $ posts
        ] `styleBasic` [ padding 10 ]

viewPostUI :: AppWenv -> AppModel -> ReceivedEvent -> AppNode
viewPostUI wenv model re = widgetTree
  where
    (Keys k xo a) = fromJust $ model ^. selectedKeys
    event = fst re
    rs = snd re
    postInfo =
      vstack
        [ hstack
            [ vstack
                [ selectableText
                  $ profileName (model ^. receivedEvents)
                  $ NostrTypes.pubKey event
                , selectableText
                  $ shortXOnlyPubKey
                  $ NostrTypes.pubKey event
                ]
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
          `styleBasic` [ padding 10 ]

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

selectableText :: Text -> WidgetNode AppModel AppEvent
selectableText t =
  textFieldD_ (WidgetValue t) [ readOnly ]
    `styleBasic` [ border 0 transparent, radius 0, bgColor $ rgbHex "#515151" ]

xOnlyPubKeyElem :: XOnlyPubKey -> WidgetNode AppModel AppEvent
xOnlyPubKeyElem x =
  hstack
    [ label "XOnlyPubKey"
    , spacer
    , textFieldD_ (WidgetValue $ T.pack $ exportXOnlyPubKey x) [ readOnly ]
    ]

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

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBg" ?~ rgbHex "#656565"
  & L.userColorMap . at "replyBg" ?~ rgbHex "#555555"
  & L.userColorMap . at "replyFg" ?~ rgbHex "#909090"
