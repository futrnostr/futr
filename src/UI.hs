{-# LANGUAGE OverloadedStrings #-}

module UI where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Crypto.Schnorr         (XOnlyPubKey, decodeHex, secKey,
                                         exportXOnlyPubKey, xOnlyPubKey)
import           Data.DateTime
import           Data.Default
import           Data.List              (sort)
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
import           Nostr.Event
import           Nostr.Keys
import           Nostr.Kind
import           Nostr.Profile
import           Nostr.Relay
import           Nostr.Request
import           UIHelpers
import qualified Widgets.EditProfile    as EditProfile
import qualified Widgets.Home           as Home
import qualified Widgets.ViewPosts      as ViewPosts
import qualified Widgets.ViewProfile    as ViewProfile

buildUI :: TChan Request -> AppWenv -> AppModel -> AppNode
buildUI channel wenv model = widgetTree
  where
    baseLayer = case model ^. currentView of
      HomeView ->
        Home.homeWidget channel homeModel
      SetupView ->
        vstack [ label "SETUP VIEW" ]
      EditProfileView ->
        EditProfile.editProfileWidget
          channel
          (fromJust $ model ^. selectedKeys)
          editProfileModel
    headerTree =
      hstack
        [ filler
        , dropdown_
            selectedKeys
            (map (\k -> Just k) (model ^. keys))
            currentKeysNode
            currentKeysNode
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
                (tooltip (relayName r) (viewCircle r) `styleBasic`
                [ cursorIcon CursorHand ])
            )
            (sort $ model ^. pool)
        addRelay =
          box_ [ alignTop, onClick $ ShowDialog NewRelayDialog ] $
          tooltip (T.pack $ "Add new relayy") $
          addIcon
        addIcon =
          icon IconPlus `styleBasic`
          [width 16, height 16, fgColor black, cursorHand]
    followingLayer = vstack []
    -- followingLayer = case model ^. selectedKeys of
    --   Nothing ->
    --     vstack []
    --   Just (Keys _ xo _ _)  ->
    --     vstack $
    --       case Map.lookup xo $ model ^. AppTypes.following of
    --         Just [] ->
    --           [ label "You don't follow anyone" ]
    --         Just ps ->
    --           map (\(Profile xo r pd) ->
    --             box_
    --               [ onClick (ViewProfile xo), alignLeft ]
    --               (profileBox xo $ name pd)
    --                 `styleBasic` [ cursorHand ]
    --           )
    --           ps
    --         Nothing ->
    --           [ label "You don't follow anyone" ]
      --where
      --  keys = fromJust $ model ^. selectedKeys
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
                    -- , hstack
                    --     [ textField searchInput
                    --         `nodeKey` "searchInput"
                    --     , spacer
                    --     , button "Search" (SearchProfile (model ^. searchInput))
                    --         `nodeEnabled` isValidPubKey
                    --     ]
                    -- , spacer
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
        -- isValidPubKey = isJust $ maybe Nothing xOnlyPubKey $ decodeHex $ view searchInput model

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
            -- DeleteEventDialog e -> viewDeleteEventDialog e
        ] `styleBasic`
      [ width 500, height 400, padding 10, radius 10, bgColor darkGray ]
  where
    ks = model ^. selectedKeys
    md = model ^. dialog
    closeIcon = icon IconClose
      `styleBasic` [ width 16, height 16, fgColor black, cursorHand ]

currentKeysNode :: Maybe Keys -> AppNode
currentKeysNode mks = case mks of
    Just (Keys _ xo _ n) ->
      case n of
        Just n' ->
          label n'
        Nothing ->
          label $ T.pack $ exportXOnlyPubKey xo
    _ ->
      label ""

-- viewPostUI :: AppWenv -> AppModel -> ReceivedEvent -> AppNode
-- viewPostUI wenv model re = widgetTree
--   where
--     (Keys _ xo _ _) = fromJust (model ^. selectedKeys)
--     event = fst re
--     rs = snd re
--     xo' = pubKey event
--     author = profileName (model ^. profiles) xo'
--     profileBox =
--       hstack
--         [ label author `styleBasic` [ textFont "Bold", textUnderline ]
--         , spacer
--         , (label $ shortXOnlyPubKey xo') `styleBasic` [ textSize 10 ]
--         ]
--     postInfo =
--       vstack
--         [ hstack
--             [ box_
--                 [ onClick $ ViewProfile xo' ] profileBox
--                 `styleBasic` [ cursorHand ]
--             , filler
--             , label ( xTimeAgo (created_at event) ( model ^. time) )
--                 `styleBasic` [ textSize 10 ]
--             ] `styleBasic` [ paddingB 10 ]
--         , hstack
--             [ label_ (content event) [ multiline, ellipsis ]
--             , filler
--             ]
--         , hstack
--             [ filler
--             , button "Delete" $ ShowDialog $ DeleteEventDialog event
--             ] `nodeVisible` (xo == xo')
--         ]
--     seenOnTree =
--       vstack $
--         map (\r -> label $ relayName r) (sort rs)
--     widgetTree =
--       vstack
--         [ label "Post Details"
--         , spacer
--         , hstack
--             [ textArea newPostInput
--                 `nodeKey` "replyPost"
--                 `styleBasic` [ height 50 ]
--             , filler
--             , button "Reply" (ReplyToPost event)
--                 `nodeEnabled` (strip (model ^. newPostInput) /= "")
--             ]
--         , spacer
--         , vscroll_ [ scrollOverlay ] $ postInfo `styleBasic` [ textTop ]
--         , filler
--         , spacer
--         , label "Seen on"
--         , spacer
--         , seenOnTree
--         ]

viewRelayDialog :: Relay -> AppModel -> AppNode
viewRelayDialog r model =
  vstack
    [ label (relayName r)
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
    , spacer `nodeVisible` (model ^. relayModel . isInvalidInput)
    , label "Invalid relay URI given" `nodeVisible` (model ^. relayModel . isInvalidInput)
    , spacer
    , button "Add relay" ValidateAndAddRelay
    ]
      `styleBasic` [ padding 10 ]

-- viewDeleteEventDialog :: Event -> AppNode
-- viewDeleteEventDialog event =
--   vstack
--     [ label "Are you sure you want to delete this event?"
--         `styleBasic` [ textFont "Bold" ]
--     , spacer
--     , hstack
--         [ label_ (content event) [ multiline, ellipsis ]
--         , filler
--         ]
--     , filler
--     , label "Delete reason:"
--     , spacer
--     , textArea deleteReason
--         `nodeKey` "deleteEventReason"
--         `styleBasic` [ height 50 ]
--     , spacer
--     , hstack
--         [ filler
--         , button "No" CloseDialog
--         , spacer
--         , mainButton "Yes" (DeleteEvent event)
--         ]
--     ]
--       `styleBasic` [ padding 10 ]


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
