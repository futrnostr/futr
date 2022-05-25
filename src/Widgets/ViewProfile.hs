{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.ViewProfile where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad.STM                    (atomically)
import           Crypto.Schnorr
import           Data.Aeson
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.DateTime
import           Data.Default
import qualified Data.List                            as List
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromJust)
import           Data.Text
import           Data.Text.Encoding                   (encodeUtf8)
import           Monomer
import qualified Monomer.Lens                         as L

import           Helpers
import           NostrFunctions
import           NostrTypes
import           UIHelpers

data ViewProfileModel =  ViewProfileModel
  { _myKeys           :: Maybe Keys
  , _xo               :: Maybe XOnlyPubKey
  , _name             :: Text
  , _about            :: Text
  , _pictureUrl       :: Text
  , _nip05Identifier  :: Text
  , _following        :: [Profile]
  , _profiles         :: Map.Map XOnlyPubKey Profile
  , _posts            :: [ReceivedEvent]
  , _time             :: DateTime
  } deriving (Eq, Show)

instance Default ViewProfileModel where
  def = ViewProfileModel Nothing Nothing "" "" "" "" [] Map.empty [] (fromSeconds 0)

data ProfileEvent
  = Follow
  | Unfollow
  deriving (Eq, Show)

makeLenses 'ViewProfileModel

handleProfileEvent
  :: TChan ServerRequest
  -> Keys
  -> WidgetEnv ViewProfileModel ProfileEvent
  -> WidgetNode ViewProfileModel ProfileEvent
  -> ViewProfileModel
  -> ProfileEvent
  -> [EventResponse ViewProfileModel ProfileEvent sp ep]
handleProfileEvent chan ks env node model evt = case evt of
  Follow ->
    [ Producer $ follow chan ks model
    , Model $ model
      & following .~ (np : model ^. following)
    ]
    where
      np = Profile
        (fromJust $ model ^. xo)
        ""
        (ProfileData
          (model ^. name)
          (model ^. about)
          (model ^. pictureUrl)
          (model ^. nip05Identifier)
        )
  Unfollow ->
    [ Producer $ unfollow chan ks model
    , Model $ model
      & following .~ newFollowing
    ]
    where
      oldFollow = model ^. xo
      newFollowing = Prelude.filter (\(Profile xo'' _ _) -> xo'' /= fromJust oldFollow) (model ^. following)

viewProfileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan ServerRequest
  -> Keys
  -> ALens' sp ViewProfileModel
  -> WidgetNode sp ep
viewProfileWidget chan keys field = composite "ViewProfileWidget" field viewProfile (handleProfileEvent chan keys)

follow :: TChan ServerRequest -> Keys -> ViewProfileModel -> (ProfileEvent -> IO ()) -> IO ()
follow chan (Keys kp xo' _ _) model sendMsg = do
  now <- getCurrentTime
  let raw = setFollowing (np : model ^. following) "" xo' now
  atomically $ writeTChan chan $ SendEvent $ signEvent raw kp xo'
  where
    np = Profile
      (fromJust $ model ^. xo)
      ""
      (ProfileData
        (model ^. name)
        (model ^. about)
        (model ^. pictureUrl)
        (model ^. nip05Identifier)
      )

unfollow :: TChan ServerRequest -> Keys -> ViewProfileModel -> (ProfileEvent -> IO ()) -> IO ()
unfollow chan (Keys kp xo' _ _) model sendMsg = do
  now <- getCurrentTime
  let raw = setFollowing newFollowing "" xo' now
  atomically $ writeTChan chan $ SendEvent $ signEvent raw kp xo'
  where
    oldFollow = model ^. xo
    newFollowing = Prelude.filter (\(Profile xo'' _ _) -> xo'' /= fromJust oldFollow) (model ^. following)

viewProfile :: WidgetEnv ViewProfileModel ProfileEvent -> ViewProfileModel -> WidgetNode ViewProfileModel ProfileEvent
viewProfile wenv model =
  vstack
    [ hstack
        [ vstack
            [ (selectableText $ model ^. name) `styleBasic` [ textSize 22 ]
            , spacer
            , (selectableText $ pack $ exportXOnlyPubKey xo') `styleBasic` [ textSize 10 ]
            , spacer
            , selectableText $ model ^. about
            ]
        , filler
        , vstack [ button btnText action ]
        ] `styleBasic` [ borderB 1 rowSepColor ]
    , spacer
--    , viewProfilePosts a b c
    ]
  where
    currentlyFollowing = List.map (\(Profile xo' _ _) -> xo') (model ^. following)
    xo' = fromJust $ model ^. xo
    action = if List.elem xo' currentlyFollowing then Unfollow else Follow
    btnText = if List.elem xo' currentlyFollowing then "Unfollow" else "Follow"
