{-# LANGUAGE OverloadedStrings    #-}

module UI where

import           Control.Lens
import           Crypto.Schnorr
import           Data.Default
import           Data.List                            (sortBy)
import           Data.Maybe
import           Monomer
import qualified Monomer.Lens                         as L
import           Monomer.Widgets.Single
import           Data.Text                            (Text, strip)
import qualified Data.Text                            as T

import           AppTypes
import           Helpers
import           NostrFunctions
import           NostrTypes

buildUI :: AppWenv -> AppModel -> AppNode
buildUI wenv model = widgetTree where
    baseLayer = viewPosts wenv model

    closeIcon = icon IconClose
        `styleBasic` [width 16, height 16, fgColor black, cursorHand]

    dialogContent = case model ^. dialog of
        NoAppDialog           -> vstack [ label "if you see this text, something went really wrong" ] -- we never show this anyway
        GenerateKeyPairDialog -> generateOrImportKeyPairStack model
        ViewPostDialog        -> viewPostUI wenv model
        ErrorReadingKeysFileDialog -> errorReadingKeysFileStack
        RelayDialog r         -> viewRelayDialog r wenv model
        NewRelayDialog        -> viewNewRelayDialog wenv model

    dialogLayer = vstack
        [ hstack
            [ filler
            , box_ [alignTop, onClick CloseDialog] closeIcon
                `nodeEnabled` (not $ elem (model ^. dialog) [GenerateKeyPairDialog, ErrorReadingKeysFileDialog])
            ]
        , spacer
        , dialogContent
        ] `styleBasic` [width 500, height 400, padding 10, radius 10, bgColor darkGray]

    widgetTree = zstack
        [ baseLayer
        , box_ [alignCenter, alignMiddle] dialogLayer
            `nodeVisible` (model ^. dialog /= NoAppDialog)
            `styleBasic` [bgColor (gray & L.a .~ 0.8)]
        ]

postRow :: AppWenv -> Int -> ReceivedEvent -> AppNode
postRow wenv idx re = row where
  e = fst re
  rowSep = rgbaHex "#A9A9A9" 0.5
  rowBg = wenv ^. L.theme . L.userColorMap . at "rowBg" . non def

  postInfo = hstack
    [ selectableText $ T.pack $ exportXOnlyPubKey $ NostrTypes.pubKey e
    , spacer
    , selectableText $ content e
    ]

  row = hstack
    [ postInfo
    , spacer
    , button "View" (ViewPost re)
    ] `styleBasic` [padding 10, borderB 1 rowSep]

viewPosts :: AppWenv -> AppModel -> AppNode
viewPosts wenv model = widgetTree where
    posts = vstack postRows where
      orderedPosts = (\e -> model ^? receivedEvents . ix e)
      postFade idx e = animRow where
        action = ViewPost e
        item = postRow wenv idx e
        animRow = animFadeOut_ [onFinished action] item `nodeKey` (content $ fst e)

      postRows = zipWith postFade [0..] (model ^. receivedEvents)

    footerTree = vstack
        [ hstack
            ([filler] ++ connections ++ [spacer, addRelay])
        ] `styleBasic` [padding 10] where
            connections = map (\r -> box_ [onClick $ ShowRelayDialog r] (tooltip (T.pack $ relayName r) (viewCircle r)
                `styleBasic` [cursorIcon CursorHand])) (sortBy sortPool (model ^. pool))
            addRelay = box_ [alignTop, onClick AddNewRelayDialog] $ tooltip (T.pack $ "Add new relayy") $ addIcon
            addIcon = icon IconPlus `styleBasic` [width 16, height 16, fgColor black, cursorHand]

    postTree = vstack
        [ label "New Post"
        , spacer
        , vstack
            [ hstack
                [ textArea newPostInput `nodeKey` "newPost"
                    `styleBasic` [height 100]
                , filler
                , button "Post" SendPost
                    `styleBasic` [height 50]
                ]
            ]
        , spacer
        , scroll_ [scrollOverlay] $ posts `styleBasic` [padding 10]
        ] `styleBasic` [padding 10]

    identitiesTree = vstack
        [ label "Identities"
        , spacer
        , vstack
            (map (\k -> xOnlyPubKeyElem $ snd' k) (model ^. keys))
        ] `styleBasic` [padding 10]

    widgetTree = vstack
        [ hstack
            [ identitiesTree
            , spacer
            , postTree
            ]
        , spacer
        , filler
        , footerTree
        ]

viewPostUI :: AppWenv -> AppModel -> AppNode
viewPostUI wenv model = widgetTree where
    k = fst' $ mainKey $ model ^. keys
    event = fst $ fromJust $ model ^. viewPost
    rs = snd $ fromJust $ model ^. viewPost
    postInfo = vstack
        [ hstack
            [ button "Back" Back
            , filler
            ]
        , spacer
        , hstack
            [ selectableText $ T.pack $ exportXOnlyPubKey $ NostrTypes.pubKey event
            , spacer
            , selectableText $ content event
            ]
        ]

    widgetTree = vstack
        ([ scroll_ [scrollOverlay] $ postInfo `styleBasic` [padding 10, textTop]
        , filler
        , hstack
            [ textArea newPostInput
                `nodeKey` "replyPost"
                `styleBasic` [height 100]
            , filler
            , button "Reply" (ReplyToPost event)
            ]
        , spacer
        , label "Seen on"
        , spacer
        ]
        ++ map (\r -> label $ T.pack $ relayName r) (sortBy sortPool rs) ++
        [ spacer
        , xOnlyPubKeyElem $ deriveXOnlyPubKey k
        ]) `styleBasic` [padding 10]

viewRelayDialog :: Relay -> AppWenv -> AppModel -> AppNode
viewRelayDialog r wenv model =
    vstack
        [ label (T.pack $ relayName r) `styleBasic` [textSize 22]
          , filler
          , hstack
            [ label "Readable"
            , spacer
            , checkbox relayReadableInput `nodeKey` "relayReadableCheckbox"
            ]
          , spacer
          , hstack
            [ label "Writable"
            , spacer
            , checkbox relayWritableInput `nodeKey` "relayWriteableCheckbox"
            ]
        , spacer
        , hstack
            [ label "Connection"
            , spacer
            , button connStatus connButton
            ]
        , spacer
        , button "Save" (UpdateRelay r)
        ] `styleBasic` [padding 10] where
            connStatus = if connected r then "Disconnect" else "Connect"
            connButton = if connected r then (DisconnectRelay r) else (ConnectRelay r)

viewNewRelayDialog :: AppWenv -> AppModel -> AppNode
viewNewRelayDialog wenv model =
    vstack
        [ hstack
            [ label "Host"
            , spacer
            , textField relayHostInput `nodeKey` "relayHostInput"
            ]
        , spacer
        , hstack
            [ label "Port"
            , spacer
            , numericField relayPortInput `nodeKey` "relayPortInput"
            ]
        , spacer
        , hstack
            [ label "Secure connection"
            , spacer
            , checkbox relaySecureInput `nodeKey` "relaySecureCheckbox"
            ]
        , spacer
        , hstack
            [ label "Readable"
            , spacer
            , checkbox relayReadableInput `nodeKey` "relayReadableCheckbox"
            ]
        , spacer
        , hstack
            [ label "Writable"
            , spacer
            , checkbox relayWritableInput `nodeKey` "relayWriteableCheckbox"
            ]
        , spacer
        , button "Add relay" AddRelay
        ] `styleBasic` [padding 10]

selectableText :: Text -> WidgetNode AppModel AppEvent
selectableText t = textFieldD_ (WidgetValue t) [readOnly]
    `styleBasic`
       [ border 0 transparent
       , radius 0
       , bgColor $ rgbHex "#515151"
       ]

xOnlyPubKeyElem :: XOnlyPubKey -> WidgetNode AppModel AppEvent
xOnlyPubKeyElem x = hstack
    [ label "XOnlyPubKey"
    , spacer
    , textFieldD_ (WidgetValue $ T.pack $ exportXOnlyPubKey x) [readOnly]
    ]

errorReadingKeysFileStack :: AppNode
errorReadingKeysFileStack =
    vstack [ label "ERROR: Keys file could not be read." ]

generateOrImportKeyPairStack :: AppModel -> AppNode
generateOrImportKeyPairStack model =
  vstack
    [ label "Welcome to nostr"
    , spacer
    , label "To get started, you need a valid key pair first"
    , spacer
    , hstack
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
    ] `styleBasic`
  [padding 10]
  where
    isValidPrivateKey =
      isJust $ maybe Nothing secKey $ decodeHex $ view mySecKeyInput model

viewCircle :: Relay -> WidgetNode AppModel AppEvent
viewCircle r = defaultWidgetNode "circlesGrid" widget where
    widget = createSingle () def {
        singleGetSizeReq = getSizeReq,
        singleRender = render
    }

    getSizeReq wenv node = (fixedSize 20, fixedSize 20)

    render wenv node renderer = do
        drawCircle renderer vp r
        where
          style = currentStyle wenv node
          vp = getContentArea node style

drawCircle
  :: Renderer -> Rect -> Relay -> IO ()
drawCircle renderer vp r = do
  let offsetX = -3
  let offsetY = 0
  let color = if connected r then paleGreen else orange
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
    rect ox oy = Rect rx ry size size where
        rx = vp ^. L.x + vp ^. L.w + ox - size
        ry = vp ^. L.y + oy

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBg" ?~ rgbHex "#656565"
  & L.userColorMap . at "replyBg" ?~ rgbHex "#555555"
  & L.userColorMap . at "replyFg" ?~ rgbHex "#909090"
