{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.EditProfile where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM            (atomically)
import Crypto.Schnorr
import Data.DateTime
import Data.Default
import Data.Map (Map)
import Data.Text
import Monomer

import qualified Data.Map as Map

import Helpers
import Nostr.Event
import Nostr.Keys
import Nostr.Profile
import Nostr.RelayPool
import Nostr.Request
import UIHelpers
import Widgets.ProfileImage

type EditProfileWenv = WidgetEnv EditProfileModel EditProfileEvent

type EditProfileNode = WidgetNode EditProfileModel EditProfileEvent

data EditProfileModel =  EditProfileModel
  { _nameInput        :: Username
  , _displayNameInput :: DisplayName
  , _aboutInput       :: About
  , _pictureInput     :: Picture
  , _currentImage     :: Picture
  , _epProfiles       :: Map XOnlyPubKey (Profile, DateTime)
  } deriving (Eq, Show)

instance Default EditProfileModel where
  def = EditProfileModel "" "" "" "" "" Map.empty

data EditProfileEvent
  = SaveProfile
  | ProfileSaved Keys Profile DateTime
  | LoadImage
  | Back
  deriving Show

makeLenses 'EditProfileModel

editProfileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> Keys
  -> (Keys -> Profile -> DateTime -> ep)
  -> ep
  -> ALens' sp EditProfileModel
  -> WidgetNode sp ep
editProfileWidget chan keys profileSaved goHome field =
  composite "editProfileWidget" field (buildUI keys) (handleProfileEvent chan keys profileSaved goHome)

handleProfileEvent
  :: (WidgetEvent ep)
  => TChan Request
  -> Keys
  -> (Keys -> Profile -> DateTime -> ep)
  -> ep
  -> EditProfileWenv
  -> EditProfileNode
  -> EditProfileModel
  -> EditProfileEvent
  -> [EventResponse EditProfileModel EditProfileEvent sp ep]
handleProfileEvent chan ks profileSaved goHome env node model evt = case evt of
  SaveProfile ->
    [ Task $ saveProfile chan ks model ]
  ProfileSaved keys profile datetime ->
    [ Report $ profileSaved keys profile datetime
    , Report goHome
    ]
  LoadImage ->
    [ Model $ model & currentImage .~ model ^. pictureInput ]
  Back ->
    [ Report $ goHome ]

saveProfile
  :: TChan Request
  -> Keys
  -> EditProfileModel
  -> IO EditProfileEvent
saveProfile requestChannel (Keys kp xo active name) model = do
  now <- getCurrentTime
  send requestChannel $ SendEvent $ signEvent (setMetadata profile xo now) kp xo
  return $ ProfileSaved (Keys kp xo active (Just name)) profile now
  where
    name = strip $ model ^. nameInput
    displayName = strip $ model ^. displayNameInput
    about = strip $ model ^. aboutInput
    picture = strip $ model ^. pictureInput
    profile = Profile name (Just displayName) (Just about) (Just picture)

buildUI :: Keys -> EditProfileWenv -> EditProfileModel -> EditProfileNode
buildUI (Keys kp xo _ _) wenv model = editView where
  myProfileImage = case model ^. currentImage of
    "" ->
      profileImage Nothing xo `styleBasic` [ width 300, height 300 ]
    pi ->
      profileImage (Just $ model ^. currentImage) xo `styleBasic` [ width 300, height 300 ]
  info = case model ^. currentImage of
    "" ->
      label "Robots lovingly delivered by Robohash.org" `styleBasic` [ textSize 8 ]
    _ ->
      filler
  formLabel t = label t `styleBasic` [ width 150 ]
  form = vstack
    [ vstack
        [ hstack [ bigLabel "Edit Account" ]
        , spacer
        , hstack [ formLabel "Username", textField nameInput `nodeKey` "username" ]
        , spacer
        , hstack [ formLabel "Display name", textField displayNameInput `nodeKey` "displayName" ]
        , spacer
        , hstack [ formLabel "About", textField aboutInput `nodeKey` "about" ]
        , spacer
        , hstack [ formLabel "Picture URL", textField pictureInput `nodeKey` "picture" ]
        , spacer
        , hstack
            [ label "Username is a required field" `styleBasic` [ textSize 9 ]
            , filler
            , button "Load image" LoadImage
            ]
        ]
        , filler
        , hstack
            [ filler
            , mainButton "Save" SaveProfile
            ]
        , spacer
        , label "Public Key"
        , spacer
        , label (pack $ exportXOnlyPubKey xo) `styleBasic` [ textSize 11 ]
        , spacer
        , label "Private Key"
        , spacer
        , label "hidden" `styleBasic` [ textSize 11 ]
    ] `styleBasic` [ paddingL 20 ]
  editView = vstack
    [ hstack
        [ vstack
            [ hstack [ button "Back" Back, filler ]
            , myProfileImage
            , spacer
            , info
            ]
        , form
        ]
    , filler
    ] `styleBasic` [ padding 10 ]
