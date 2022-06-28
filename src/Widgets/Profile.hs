{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Profile where

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
import Data.List (filter)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, pack, unpack)
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

type ProfileWenv = WidgetEnv ProfileModel ProfileEvent

type ProfileNode = WidgetNode ProfileModel ProfileEvent

data ProfileModel = ProfileModel
  { _profile      :: Maybe XOnlyPubKey
  , _futr         :: FutrModel
  , _subscription :: Maybe SubscriptionId
  } deriving (Eq, Show)

instance Default ProfileModel where
  def = ProfileModel Nothing def Nothing

data ProfileEvent
  = Follow
  | Unfollow
  | ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  | Back
  | StartSubscription
  | SubscriptionStarted SubscriptionId
  | StopSubscription
  | NewResponses [(Response, Relay)]
  deriving Show

makeLenses 'ProfileModel

profileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => MVar RelayPool
  -> TChan Request
  -> (ep)
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> ALens' sp ProfileModel
  -> WidgetNode sp ep
profileWidget pool request back viewPostDetails viewProfile follow unfollow model =
  composite_
    "ProfileWidget"
    model
    buildUI
    (handleProfileEvent pool request back viewPostDetails viewProfile follow unfollow)
    [ onInit StartSubscription, onDispose StopSubscription ]

handleProfileEvent
  :: MVar RelayPool
  -> TChan Request
  -> ep
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> ProfileWenv
  -> ProfileNode
  -> ProfileModel
  -> ProfileEvent
  -> [EventResponse ProfileModel ProfileEvent sp ep]
handleProfileEvent pool request back viewPostDetails viewProfile follow unfollow env node model evt = case evt of
  Follow ->
    [ Report $ follow $ fromJust $ model ^. profile ]
  Unfollow ->
    [ Report $ unfollow $ fromJust $ model ^. profile ]
  ViewPostDetails re ->
    [ Report $ viewPostDetails re ]
  ViewProfile xo ->
    [ Report $ viewProfile xo ]
  Back ->
    [ Report $ back ]
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

startSubscription
  :: MVar RelayPool
  -> TChan Request
  -> ProfileModel
  -> (ProfileEvent -> IO ())
  -> IO ()
startSubscription pool request model sendMsg = do
  now <- getCurrentTime
  let filters = [ MetadataFilter [ profileKey ] now, TextNoteFilter [ profileKey ] now ]
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
    profileKey = fromJust $ model ^. profile

stopSubscription :: MVar RelayPool -> TChan Request -> Maybe SubscriptionId -> IO ()
stopSubscription pool request subId = do
  case subId of
    Just subId' ->
      unsubscribe pool request subId'
    Nothing ->
      return ()

buildUI
  :: ProfileWenv
  -> ProfileModel
  -> ProfileNode
buildUI wenv model =
  vstack
    [ hstack
        [ vstack [ button "Back" Back ]
        , spacer
        , profileImage
            (Futr.pictureUrl (model ^. futr . profiles) profileKey)
            profileKey
            Medium
        , vstack
            [ (selectableText $ name) `styleBasic` [ textSize 22 ]
            , (selectableText $ pack $ exportXOnlyPubKey profileKey) `styleBasic` [ textSize 10 ]
            , selectableText $ fromMaybe "" displayName
            , selectableText $ pack $ "About: " ++ (unpack $ fromMaybe "" about)
            ]
        , if xo /= profileKey then vstack [ button btnText action ] else filler
        ]
    , spacer
    , hstack
        [ filler
        , case toSeconds at of
            0 -> label "No profile data found"
            _ -> label $ pack $ "Last updated " ++ (unpack $ xTimeAgo at (model ^. futr . time))
        ]
    , spacer
    , label "Recent posts"  `styleBasic` [ paddingB 10, paddingT 15, borderB 1 rowSepColor ]
    , ViewPosts.viewPosts
        ViewPostDetails
        ViewProfile
        wenv
        (model ^. futr)
        (filter (\re -> kind (fst re) == TextNote && NE.pubKey (fst re) == profileKey) (model ^. futr . events))
    ]
  where
    profileKey = fromJust $ model ^. profile
    ((Profile.Profile name displayName about _), at) = fromMaybe (def, fromSeconds 0) $ Map.lookup profileKey (model ^. futr . profiles)
    (year, month, day, hour, min, _) = toGregorian $ at
    (Keys _ xo _ user) = fromJust $ model ^. futr . selectedKeys
    action = if List.elem profileKey (model ^. futr . contacts) then Unfollow else Follow
    btnText = if List.elem profileKey (model ^. futr . contacts) then "Unfollow" else "Follow"
