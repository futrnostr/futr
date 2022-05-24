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
import NostrTypes

selectableText :: WidgetEvent e => Text -> WidgetNode s e
selectableText t =
  textFieldD_ (WidgetValue t) [ readOnly ]
    `styleBasic` [ border 0 transparent, radius 0, bgColor $ rgbHex "#515151" ]


xOnlyPubKeyElem :: WidgetEvent e => XOnlyPubKey -> WidgetNode s e
xOnlyPubKeyElem x =
  hstack
    [ label "XOnlyPubKey"
    , spacer
    , textFieldD_ (WidgetValue $ T.pack $ exportXOnlyPubKey x) [ readOnly ]
    ]

profileName :: Map.Map XOnlyPubKey Profile -> XOnlyPubKey -> Text
profileName m xo =
  case Map.lookup xo m of
    Just (Profile xo' r pd) ->
      pdName pd
    Nothing ->
      ""

shortXOnlyPubKey :: XOnlyPubKey -> Text
shortXOnlyPubKey xo = T.pack
  $ part1 ++ ".." ++ part2
  where
    str = exportXOnlyPubKey xo
    part1 = take 4 str
    part2 = take 4 $ reverse str

rowSepColor :: Color
rowSepColor = rgbaHex "#A9A9A9" 0.75

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBg" ?~ rgbHex "#656565"
  & L.userColorMap . at "replyBg" ?~ rgbHex "#555555"
  & L.userColorMap . at "replyFg" ?~ rgbHex "#909090"

viewPosts
  :: (WidgetModel s, WidgetEvent e)
  => WidgetEnv s e
  -> Map.Map XOnlyPubKey Profile
  -> [ReceivedEvent]
  -> ALens' s Text
  -> DateTime
  -> (ReceivedEvent -> e)
  -> e
  -> (XOnlyPubKey -> e)
  -> WidgetNode s e
viewPosts
  wenv profiles receivedEvents newPostInput
  time viewDetailsAction sendPostAction viewProfileAction = widgetTree
  where
    posts = vstack postRows
      where
        -- display only kind 1 events
        orderedPosts = filter (\re -> kind (fst re) == 1) receivedEvents
        postFade idx ev = animRow
          where
            item = postRow wenv profiles idx ev time viewDetailsAction viewProfileAction
            animRow =
              animFadeOut_ [] item `nodeKey` (content $ fst ev)
        postRows = zipWith postFade [ 0 .. ] orderedPosts
    wdata = WidgetLens newPostInput
    newPostInputData = widgetDataGet (_weModel wenv) wdata
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
                , button "Post" sendPostAction
                    `nodeEnabled` (strip newPostInputData /= "")
                ]
            ]
        , spacer
        , scroll_ [ scrollOverlay ] posts
        ]

postRow
  :: (WidgetModel s, WidgetEvent e)
  => WidgetEnv s e
  -> Map.Map XOnlyPubKey Profile
  -> Int
  -> ReceivedEvent
  -> DateTime
  -> (ReceivedEvent -> e)
  -> (XOnlyPubKey -> e)
  -> WidgetNode s e
postRow wenv m idx re time viewDetailsAction viewProfileAction = row
  where
    event = fst re
    xo = NostrTypes.pubKey event
    rowBg = wenv ^. L.theme . L.userColorMap . at "rowBg" . non def
    profileBox =
      vstack
        [ label $ profileName m xo
        , spacer
        , (label $ shortXOnlyPubKey xo) `styleBasic` [textSize 10]
        ]
    row =
      vstack
        [ hstack
            [ filler
            , (label $ xTimeAgo (created_at event) time)
                `styleBasic` [ textSize 10 ]
            ]
        , hstack
            [ box_ [ onClick (viewProfileAction xo) ] profileBox
                `styleBasic` [ cursorHand ]
            , spacer
            , selectableText $ content event
            , spacer
            , vstack
                [ spacer
                , button "Details" $ viewDetailsAction re
                , spacer
                ]
            ]
        ] `styleBasic` [ paddingT 10, paddingB 10, paddingR 20, borderB 1 rowSepColor ]
