{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.ViewProfile where

import Debug.Trace

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM              (atomically)
import Crypto.Schnorr
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.Maybe                     (fromJust, fromMaybe)
import Data.Text
import Data.Text.Encoding             (encodeUtf8)
import Monomer

import qualified Data.List            as List
import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Monomer.Lens         as L

import Futr
import Helpers
import Nostr.Event             as NE
import Nostr.Keys
import Nostr.Kind
import Nostr.Request
import UIHelpers
import Widgets.ProfileImage

import qualified Nostr.Profile as Profile
import qualified Widgets.ViewPosts as ViewPosts

type ViewProfileWenv = WidgetEnv ViewProfileModel ProfileEvent

type ViewProfileNode = WidgetNode ViewProfileModel ProfileEvent

data ViewProfileModel = ViewProfileModel
  { _profile          :: Maybe XOnlyPubKey
  , _following        :: Map.Map XOnlyPubKey [Profile.Profile]
  , _futr             :: FutrModel
  } deriving (Eq, Show)

instance Default ViewProfileModel where
  def = ViewProfileModel Nothing Map.empty def

data ProfileEvent
  = Follow
  | Unfollow
  | ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  | Back
  deriving Show

makeLenses 'ViewProfileModel

viewProfileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> (ep)
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> ALens' sp ViewProfileModel
  -> WidgetNode sp ep
viewProfileWidget chan back viewPostDetails viewProfile follow unfollow model =
  composite
    "ViewProfileWidget"
    model
    buildUI
    (handleProfileEvent chan back viewPostDetails viewProfile follow unfollow)

handleProfileEvent
  :: TChan Request
  -> ep
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> (XOnlyPubKey -> ep)
  -> ViewProfileWenv
  -> ViewProfileNode
  -> ViewProfileModel
  -> ProfileEvent
  -> [EventResponse ViewProfileModel ProfileEvent sp ep]
handleProfileEvent chan back viewPostDetails viewProfile follow unfollow env node model evt = case evt of
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

buildUI
  :: ViewProfileWenv
  -> ViewProfileModel
  -> ViewProfileNode
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
        , selectableText $ pack $ "Last updated " ++ (unpack $ xTimeAgo at (model ^. futr . time))
        ]
    , spacer
    , label "Recent posts"  `styleBasic` [ paddingB 10, paddingT 15, borderB 1 rowSepColor ]
    , ViewPosts.viewPosts
        (\re -> kind (fst re) == TextNote && NE.pubKey (fst re) == profileKey)
        ViewPostDetails
        ViewProfile
        wenv
        (model ^. futr)
    ]
  where
    profileKey = fromJust $ model ^. profile
    ((Profile.Profile name displayName about _), at) = fromMaybe (def, fromSeconds 0) $ Map.lookup profileKey (model ^. futr . profiles)
    (year, month, day, hour, min, _) = toGregorian $ at
    (Keys _ xo _ user) = fromJust $ model ^. futr . selectedKeys
    action = if List.elem profileKey (model ^. futr . contacts) then Unfollow else Follow
    btnText = if List.elem profileKey (model ^. futr . contacts) then "Unfollow" else "Follow"
