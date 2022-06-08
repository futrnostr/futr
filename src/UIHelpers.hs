{-# LANGUAGE OverloadedStrings #-}

module UIHelpers where

import           Control.Lens
import           Crypto.Schnorr
import           Data.DateTime
import           Data.Default
import qualified Data.Map               as Map
import           Data.Text              (Text, strip)
import qualified Data.Text              as T
import           Monomer
import qualified Monomer.Lens           as L

import Helpers
import Nostr.Profile

data ImageSize = Big | Small | Mini
  deriving (Eq)

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

fallbackProfileImage :: WidgetEvent e => Maybe XOnlyPubKey -> ImageSize -> WidgetNode s e
fallbackProfileImage xo size = maybe (label "a") img xo where
  baseUrl = "https://robohash.org/<xo>.png"
  imgSize = case size of
    Big   -> [ width 300, height 300 ]
    Small -> [ width 100, height 100 ]
    Mini  -> [ width 50, height 50 ]
  imgUrl x = T.replace "<xo>" (T.pack $ exportXOnlyPubKey x) baseUrl
  img x = image_ (imgUrl x) [ fitFill ] `styleBasic` imgSize

-- profileName :: Map.Map XOnlyPubKey Profile -> XOnlyPubKey -> Text
-- profileName m xo =
--   case Map.lookup xo m of
--     Just (Profile xo' r pd) ->
--       name pd
--     Nothing ->
--       ""

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
