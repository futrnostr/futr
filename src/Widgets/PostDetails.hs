{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.PostDetails where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad (forever, void)
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
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response
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
  , _subscription :: Maybe SubscriptionId
  } deriving (Eq, Show)

instance Default PostDetailsModel where
  def = PostDetailsModel Nothing def "" False "" Nothing

data PostDetailsEvent
  = ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  | Back
  | ReplyToPost
  | ShowDeleteEventDialog
  | CloseDialog
  | DeleteEvent
  | StartSubscription
  | SubscriptionStarted SubscriptionId
  | StopSubscription
  | NewResponses [(Response, Relay)]
  deriving Show

makeLenses 'PostDetailsModel

postDetailsWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => MVar RelayPool
  -> TChan Request
  -> (ep)
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> (Event -> Text -> ep)
  -> ALens' sp PostDetailsModel
  -> WidgetNode sp ep
postDetailsWidget pool request back viewPostDetails postDetails replyToPost model =
  composite_
    "PostDetailsWidget"
    model
    buildUI
    (handlePostDetailsEvent pool request back viewPostDetails postDetails replyToPost)
    [ onInit StartSubscription, onDispose StopSubscription ]

handlePostDetailsEvent
  :: MVar RelayPool
  -> TChan Request
  -> ep
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> (Event -> Text -> ep)
  -> PostDetailsWenv
  -> PostDetailsNode
  -> PostDetailsModel
  -> PostDetailsEvent
  -> [EventResponse PostDetailsModel PostDetailsEvent sp ep]
handlePostDetailsEvent pool request back viewPostDetails viewProfile replyToPost env node model evt = case evt of
  ViewPostDetails re ->
    [ Report $ viewPostDetails re ]
  ViewProfile xo ->
    [ Report $ viewProfile xo ]
  Back ->
    [ Report back ]
  ReplyToPost ->
    [ Model $ model & newPostInput .~ ""
    , Report $ replyToPost (fst $ fromJust $ model ^. event) (model ^. newPostInput)
    ]
  ShowDeleteEventDialog ->
    [ Model $ model & deleteDialog .~ True ]
  CloseDialog ->
    [ Model $ model & deleteDialog .~ False ]
  DeleteEvent ->
    [ Model $ model & deleteDialog .~ False
    , voidTask $ deleteEvent request model
    , Report back
    ]
  StartSubscription ->
    [ Producer $ startSubscription pool request model ]
  SubscriptionStarted subId ->
    [ Model $ model & subscription .~ Just subId ]
  StopSubscription ->
    [ Model $ model & subscription .~ Nothing
    , voidTask $ stopSubscription pool request (model ^. subscription)
    ]
  NewResponses responseList ->
    [ Model $ model & futr .~ newFutr (model ^. futr) responseList ]

deleteEvent :: TChan Request -> PostDetailsModel -> IO ()
deleteEvent request model = do
  now <- getCurrentTime
  let unsigned = deleteEvents [eventId e] reason xo now
  atomically $ writeTChan request $ SendEvent $ signEvent unsigned kp xo
  where
    (Keys kp xo _ _) = fromJust $ model ^. futr . selectedKeys
    e = fst $ fromJust $ model ^. event
    reason = model ^. deleteReason

startSubscription
  :: MVar RelayPool
  -> TChan Request
  -> PostDetailsModel
  -> (PostDetailsEvent -> IO ())
  -> IO ()
startSubscription pool request model sendMsg = do
  now <- getCurrentTime
  let filters = [ MetadataFilter [ author ] now, LinkedEvents [ eid ] now ]
  response <- atomically newTChan
  subId <- subscribe pool request response filters
  sendMsg $ SubscriptionStarted subId
  void . forever $ do
    msg <- atomically $ readTChan response
    msgs <- collectJustM . atomically $ tryReadTChan response
    sendMsg $ NewResponses (msg : msgs)
    threadDelay $ 100 * 1000 -- to avoid re-rendering, we only send 10 times per second new data in batches to the UI
  where
    (Keys _ xo _ _) = fromJust $ model ^. futr . selectedKeys
    event' = fst $ fromJust $ model ^. event
    eid = eventId event'
    author = NE.pubKey event'

stopSubscription :: MVar RelayPool -> TChan Request -> Maybe SubscriptionId -> IO ()
stopSubscription pool request subId = do
  case subId of
    Just subId' ->
      unsubscribe pool request subId'
    Nothing ->
      return ()

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
        [ vscroll_ [ scrollOverlay ] $ vstack
            [ postInfo `styleBasic` [ textTop ]
            , spacer
            , hstack
                [ textArea newPostInput
                    `nodeKey` "replyPost"
                    `styleBasic` [ height 50 ]
                , filler
                , button "Reply" ReplyToPost
                    `nodeEnabled` (strip (model ^. newPostInput) /= "")
                ]
            , spacer
            , label "Comments" `styleBasic` [ paddingB 10, paddingT 15, borderB 1 rowSepColor ]
            , spacer
            , ViewPosts.viewPosts
                ViewPostDetails
                ViewProfile
                wenv
                (model ^. futr)
                (filter (\re -> kind (fst re) == TextNote) (model ^. futr . events))
            , filler
            , label "Seen on"
            , spacer
            , seenOnTree
            ] `styleBasic` [ paddingR 10 ]
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
