{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.PostDetails where

import Debug.Trace

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM (atomically)
import Crypto.Schnorr
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (encodeUtf8)
import Monomer

import qualified Data.List            as List
import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Monomer.Lens         as L

import Futr
import Helpers
import Nostr.Event as NE
import Nostr.Keys
import Nostr.Kind
import Nostr.Relay
import Nostr.Request
import UIHelpers
import Widgets.ProfileImage

import qualified Nostr.Profile as Profile
import qualified Widgets.ViewPosts as ViewPosts

type PostDetailsWenv = WidgetEnv PostDetailsModel PostDetailsEvent

type PostDetailsNode = WidgetNode PostDetailsModel PostDetailsEvent

data PostDetailsModel = PostDetailsModel
  { _event        :: Maybe ReceivedEvent
  , _futr         :: FutrModel
  , _newPostInput :: Text
  , _deleteDialog :: Bool
  , _deleteReason :: Text
  } deriving (Eq, Show)

instance Default PostDetailsModel where
  def = PostDetailsModel Nothing def "" False ""

data PostDetailsEvent
  = ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  | Back
  | ShowDeleteEventDialog
  | CloseDialog
  | DeleteEvent
  deriving Show

makeLenses 'PostDetailsModel

postDetailsWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> (ep)
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> ALens' sp PostDetailsModel
  -> WidgetNode sp ep
postDetailsWidget chan back viewPostDetails postDetails model =
  composite
    "PostDetailsWidget"
    model
    buildUI
    (handlePostDetailsEvent chan back viewPostDetails postDetails)

handlePostDetailsEvent
  :: TChan Request
  -> ep
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> PostDetailsWenv
  -> PostDetailsNode
  -> PostDetailsModel
  -> PostDetailsEvent
  -> [EventResponse PostDetailsModel PostDetailsEvent sp ep]
handlePostDetailsEvent request back viewPostDetails viewProfile env node model evt = case evt of
  ViewPostDetails re ->
    [ Report $ viewPostDetails re ]
  ViewProfile xo ->
    [ Report $ viewProfile xo ]
  Back ->
    [ Report back ]
  ShowDeleteEventDialog ->
    [ Model $ model & deleteDialog .~ True ]
  CloseDialog ->
    [ Model $ model & deleteDialog .~ False ]
  DeleteEvent ->
    [ Model $ model & deleteDialog .~ False
    , voidTask $ deleteEvent request model
    , Report back
    ]

deleteEvent :: TChan Request -> PostDetailsModel -> IO ()
deleteEvent request model = do
  now <- getCurrentTime
  let unsigned = deleteEvents [eventId e] reason xo now
  atomically $ writeTChan request $ SendEvent $ signEvent unsigned kp xo
  where
    (Keys kp xo _ _) = fromJust $ model ^. futr . selectedKeys
    e = fst $ fromJust $ model ^. event
    reason = model ^. deleteReason

buildUI
  :: PostDetailsWenv
  -> PostDetailsModel
  -> PostDetailsNode
buildUI wenv model = widgetTree
  where
    (Keys _ xo _ _) = fromJust (model ^. futr . selectedKeys)
    event' = fst $ fromJust $ model ^. event
    rs = snd $ fromJust $ model ^. event
    profileKey = NE.pubKey event'
    ((Profile.Profile name displayName about _), at) = fromMaybe (def, fromSeconds 0) $ Map.lookup profileKey (model ^. futr . profiles)
    profileBox = hstack
      [ profileImage
          (Futr.pictureUrl (model ^. futr . profiles) profileKey)
          profileKey
          Small
      , vstack
          [ (selectableText $ name) `styleBasic` [ textSize 22 ]
          , (selectableText $ pack $ exportXOnlyPubKey profileKey) `styleBasic` [ textSize 10 ]
          , selectableText $ fromMaybe "" displayName
          ]
      ]
    postInfo =
      vstack
        [ hstack
            [ vstack [ button "Back" Back ]
            , spacer
            , box_
                [ onClick $ ViewProfile profileKey ] profileBox
                `styleBasic` [ cursorHand ]
            , filler
            , label ( xTimeAgo (created_at event') ( model ^. futr . time) )
                `styleBasic` [ textSize 10 ]
            ] `styleBasic` [ paddingB 10 ]
        , hstack
            [ label_ (content event') [ multiline, ellipsis ]
            , filler
            , button "Delete" ShowDeleteEventDialog
                `nodeVisible` (xo == profileKey)
            ]
        ]
    seenOnTree =
      vstack $
        map (\r -> label $ relayName r) (sort rs)
    widgetTree =
      zstack
        [ vstack
            [ vscroll_ [ scrollOverlay ] $ postInfo `styleBasic` [ textTop ]
            , spacer
            , hstack
                [ textArea newPostInput
                    `nodeKey` "replyPost"
                    `styleBasic` [ height 50 ]
                , filler
                --, button "Reply" (ReplyToPost event')
                  --  `nodeEnabled` (strip (model ^. newPostInput) /= "")
                ]
            , spacer
            , filler
            , label "Seen on"
            , spacer
            , seenOnTree
            ]
        , box_ [ alignCenter, alignMiddle ] (viewDeleteEventDialog event')
            `nodeVisible` (model ^. deleteDialog == True)
            `styleBasic` [ bgColor (gray & L.a .~ 0.8) ]
        ]

viewDeleteEventDialog :: Event -> PostDetailsNode
viewDeleteEventDialog event = vstack
  [ hstack
      [ bigLabel "Delete event"
      , filler
      , box_ [alignTop, onClick CloseDialog ] closeIcon
      ]
  , spacer
  , vstack
      [  label "Are you sure you want to delete this event?"
          `styleBasic` [ textFont "Bold" ]
      , spacer
      , hstack
          [ label_ (content event) [ multiline, ellipsis ]
          , filler
          ]
      , filler
      , label "Delete reason:"
      , spacer
      , textArea deleteReason
          `nodeKey` "deleteEventReason"
          `styleBasic` [ height 50 ]
      , spacer
      , hstack
          [ filler
          , button "No" CloseDialog
          , spacer
          , mainButton "Yes" DeleteEvent
          ]
      ] `styleBasic` [ padding 10 ]
  ] `styleBasic` [ width 500, height 250, padding 10, radius 10, bgColor darkGray ]
  where
    closeIcon = icon IconClose
      `styleBasic` [ width 16, height 16, fgColor black, cursorHand ]
