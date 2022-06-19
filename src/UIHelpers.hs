{-# LANGUAGE OverloadedStrings #-}

module UIHelpers where

import Control.Lens
import Crypto.Schnorr
import Data.DateTime
import Data.Default
import Data.Text (Text, strip)
import Monomer
import Monomer
import Monomer.Widgets.Single

import qualified Data.Text    as T
import qualified Data.Map     as Map
import qualified Monomer.Lens as L

import Helpers
import Nostr.Profile
import Nostr.Relay

voidTask :: IO () -> EventResponse s e sp ep
voidTask action = Producer (const action)

selectableText :: WidgetEvent e => Text -> WidgetNode s e
selectableText t =
  textFieldD_ (WidgetValue t) [ readOnly ]
    `styleBasic` [ border 0 transparent, radius 0, bgColor $ rgbHex "#515151" ]

xOnlyPubKeyElem :: WidgetEvent e => XOnlyPubKey -> WidgetNode s e
xOnlyPubKeyElem xo =
  hstack
    [ label "XOnlyPubKey"
    , spacer
    , textFieldD_ (WidgetValue $ T.pack $ exportXOnlyPubKey xo) [ readOnly ]
    ]

bigLabel :: Text -> WidgetNode s e
bigLabel text =
  label text `styleBasic` [ textSize 20, textFont "Bold" ]

profileBox :: (WidgetModel s, WidgetEvent e) => XOnlyPubKey -> Text -> WidgetNode s e
profileBox xo name =
  vstack
    [ label name
    , spacer
    , (label $ shortXOnlyPubKey xo) `styleBasic` [textSize 10]
    ]

rowSepColor :: Color
rowSepColor = rgbaHex "#A9A9A9" 0.75

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBg" ?~ rgbHex "#656565"
  & L.userColorMap . at "replyBg" ?~ rgbHex "#555555"
  & L.userColorMap . at "replyFg" ?~ rgbHex "#909090"

viewCircle :: (WidgetModel sp, WidgetEvent ep) => Relay -> WidgetNode sp ep
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
