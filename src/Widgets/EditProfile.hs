{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.EditProfile where

import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM            (atomically)
import Crypto.Schnorr
import Data.DateTime
import Data.Default
import Data.Text
import Monomer

import Helpers
import Nostr.Event
import Nostr.Keys
import Nostr.Profile
import Nostr.RelayPool
import Nostr.Request
import UIHelpers

type EditProfileWenv = WidgetEnv EditProfileModel EditProfileEvent

type EditProfileNode = WidgetNode EditProfileModel EditProfileEvent

data EditProfileModel =  EditProfileModel
  { _nameInput        :: Text
  , _displayNameInput :: Text
  , _aboutInput       :: Text
  , _pictureInput     :: Text
  } deriving (Eq, Show)

instance Default EditProfileModel where
  def = EditProfileModel "" "" "" ""

data EditProfileEvent
  = SaveProfile
  | ProfileSaved MetadataContent
  | Back
  deriving (Eq, Show)

makeLenses 'EditProfileModel

editProfileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> Keys
  -> (MetadataContent -> ep)
  -> ep
  -> ALens' sp EditProfileModel
  -> WidgetNode sp ep
editProfileWidget chan keys profileSaved goHome field =
  composite "editProfileWidget" field viewProfile (handleProfileEvent chan keys profileSaved goHome)

handleProfileEvent
  :: (WidgetEvent ep)
  => TChan Request
  -> Keys
  -> (MetadataContent -> ep)
  -> ep
  -> EditProfileWenv
  -> EditProfileNode
  -> EditProfileModel
  -> EditProfileEvent
  -> [EventResponse EditProfileModel EditProfileEvent sp ep]
handleProfileEvent chan ks profileSaved goHome env node model evt = case evt of
  SaveProfile ->
    [ Task $ saveProfile chan ks model ]
  ProfileSaved metadataContent ->
    [ Report $ profileSaved metadataContent ]
  Back ->
    [ Report $ goHome ]

saveProfile
  :: TChan Request
  -> Keys
  -> EditProfileModel
  -> IO EditProfileEvent
saveProfile requestChannel (Keys kp xo _ _) model = do
  now <- getCurrentTime
  send requestChannel $ SendEvent $ signEvent (setMetadata metadataContent xo now) kp xo
  return $ ProfileSaved metadataContent
  where
    name = strip $ model ^. nameInput
    displayName = strip $ model ^. displayNameInput
    about = strip $ model ^. aboutInput
    picture = strip $ model ^. pictureInput
    metadataContent = MetadataContent name (Just displayName) (Just about) (Just picture)

viewProfile :: EditProfileWenv -> EditProfileModel -> EditProfileNode
viewProfile wenv model =
  vstack
    [ hstack [ button "Back" Back, filler, bigLabel "Edit Profile", filler ]
    , filler
    , hstack
        [ label "Name"
        , filler
        , tf nameInput "name"
        ]
    , spacer
    , hstack
        [ label "Display Name"
        , filler
        , tf displayNameInput "displayName"
        ]
    , spacer
    , hstack
        [ label "About"
        , filler
        , tf aboutInput "about"
        ]
    , spacer
    , hstack
        [ label "Picture URL"
        , filler
        , tf pictureInput "picture"
        ]
    , filler
    , mainButton "Save" SaveProfile
    , filler
    ] `styleBasic` [ padding 20 ]
    where
      tf input id' = textField input `nodeKey` id' `styleBasic` [ width 400 ]
