{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.ViewProfile where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM              (atomically)
import Crypto.Schnorr
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.Maybe                     (fromJust)
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
  { _futr             :: FutrModel
  , _xo               :: Maybe XOnlyPubKey
  , _name             :: Text
  , _displayName      :: Text
  , _about            :: Text
  , _pictureUrl       :: Text
  , _following        :: Map.Map XOnlyPubKey [Profile.Profile]
  } deriving (Eq, Show)

instance Default ViewProfileModel where
  def = ViewProfileModel def Nothing  "" "" "" "" Map.empty

data ProfileEvent
  = Follow
  | Unfollow
  | ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  | Back
  deriving Show

makeLenses 'ViewProfileModel

handleProfileEvent
  :: TChan Request
  -> FutrModel
  -> ep
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> ViewProfileWenv
  -> ViewProfileNode
  -> ViewProfileModel
  -> ProfileEvent
  -> [EventResponse ViewProfileModel ProfileEvent sp ep]
handleProfileEvent chan futr back viewPostDetails viewProfile env node model evt = case evt of
--  Follow ->
--    [ Producer $ follow chan ks model
--    , Model $ model
--      & following .~ newFollowing'
--    ]
--    where
--      xo' = fromJust $ model ^. xo
--      np = Profile.Profile
--        xo'
--        ""
--        (Profile.ProfileData
--          (model ^. name)
--          (model ^. about)
--          (model ^. pictureUrl)
--          (model ^. nip05Identifier)
--        )
--      oldFollowing = Map.findWithDefault [] xo' (model ^. following)
--      newFollowing = np : oldFollowing
--      newFollowing' = Map.insert xo' newFollowing (model ^. following)
--  Unfollow ->
--    [ Producer $ unfollow chan ks model
--    , Model $ model
--        & following .~ newFollowing'
--    ]
--    where
--      xo' = fromJust $ model ^. xo
--      oldFollowing = Map.findWithDefault [] xo' (model ^. following)
--      newFollowing = Prelude.filter (\(Profile.Profile xo'' _ _) -> xo'' /= xo') oldFollowing
--      newFollowing' = Map.insert xo' newFollowing (model ^. following)
  ViewPostDetails re ->
    [ Report $ viewPostDetails re ]
  ViewProfile xo ->
    [ Report $ viewProfile xo ]
  Back ->
    [ Report $ back ]

viewProfileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> FutrModel
  -> (ep)
  -> (ReceivedEvent -> ep)
  -> (XOnlyPubKey -> ep)
  -> ALens' sp ViewProfileModel
  -> WidgetNode sp ep
viewProfileWidget chan futr back viewPostDetailsAction viewProfileAction model =
  composite
    "ViewProfileWidget"
    model
    buildUI
    (handleProfileEvent chan futr back viewPostDetailsAction viewProfileAction)

--follow :: TChan Request -> Keys -> ViewProfileModel -> (ProfileEvent -> IO ()) -> IO ()
--follow chan (Keys kp xo' _ _) model sendMsg = do
--  now <- getCurrentTime
--  let raw = setContacts (np : oldFollowing) "" xo' now
--  atomically $ writeTChan chan $ SendEvent $ signEvent raw kp xo'
--  where
--    oldFollowing = Map.findWithDefault [] xo' (model ^. following)
--    np = Profile.Profile
--      (fromJust $ model ^. xo)
--      ""
--      (Profile.ProfileData
--        (model ^. name)
--        (model ^. about)
--        (model ^. pictureUrl)
--        (model ^. nip05Identifier)
--      )
--
--unfollow :: TChan Request -> Keys -> ViewProfileModel -> (ProfileEvent -> IO ()) -> IO ()
--unfollow chan (Keys kp xo' _ _) model sendMsg = do
--  now <- getCurrentTime
--  let raw = setContacts newFollowing "" xo' now
--  atomically $ writeTChan chan $ SendEvent $ signEvent raw kp xo'
--  where
--    oldFollow = fromJust $ model ^. xo
--    oldFollowing = Map.findWithDefault [] xo' (model ^. following)
--    newFollowing = Prelude.filter (\(Profile.Profile xo'' _ _) -> xo'' /= oldFollow) oldFollowing

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
            (Futr.pictureUrl (model ^. futr. profiles) (fromJust $ model ^. xo))
            (fromJust $ model ^. xo)
            Medium
        , vstack
            [ (selectableText $ model ^. name) `styleBasic` [ textSize 22 ]
            , (selectableText $ pack $ exportXOnlyPubKey xo') `styleBasic` [ textSize 10 ]
            , selectableText $ model ^. about
            ]
        , filler
        --, vstack [ button btnText action ]
        ]
    , spacer
    , label "Recent posts"  `styleBasic` [ paddingB 10, paddingT 15, borderB 1 rowSepColor ]
    , ViewPosts.viewPosts
        (\re -> kind (fst re) == TextNote && NE.pubKey (fst re) == xo')
        ViewPostDetails
        ViewProfile
        wenv
        (model ^. futr)
    ]
  where
    (Keys _ user _ _) = fromJust $ model ^. futr . selectedKeys
    xo' = fromJust $ model ^. xo
--    currentlyFollowing = Map.findWithDefault [] user (model ^. following)
--    currentlyFollowing' = List.map extractXOFromProfile currentlyFollowing
--    action = if List.elem xo' currentlyFollowing' then Unfollow else Follow
--    btnText = if List.elem xo' currentlyFollowing' then "Unfollow" else "Follow"
