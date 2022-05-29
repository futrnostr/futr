{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.EditProfile where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad.STM                    (atomically)
import           Crypto.Schnorr
import           Data.DateTime
import           Data.Default
import           Data.Text
import           Monomer

import Helpers
import Nostr.Event
import Nostr.Keys
import Nostr.Profile
import Nostr.Request
import NostrFunctions

data ProfileModelInputs = ProfileModelInputs
  { _nameInput            :: Text
  , _aboutInput           :: Text
  , _pictureUrlInput      :: Text
  , _nip05IdentifierInput :: Text
  , _errors               :: [Text]
} deriving (Eq, Show)

instance Default ProfileModelInputs where
  def = ProfileModelInputs "" "" "" "" []

data EditProfileModel =  EditProfileModel
  { _name            :: Text
  , _about           :: Text
  , _pictureUrl      :: Text
  , _nip05Identifier :: Text
  , _inputs          :: ProfileModelInputs
  } deriving (Eq, Show)

instance Default EditProfileModel where
  def = EditProfileModel "" "" "" "" def


data ProfileEvent
  = SaveProfile
  deriving (Eq, Show)

makeLenses 'ProfileModelInputs
makeLenses 'EditProfileModel

handleProfileEvent
  :: TChan Request
  -> Keys
  -> WidgetEnv EditProfileModel ProfileEvent
  -> WidgetNode EditProfileModel ProfileEvent
  -> EditProfileModel
  -> ProfileEvent
  -> [EventResponse EditProfileModel ProfileEvent sp ep]
handleProfileEvent chan ks env node model evt = case evt of
  SaveProfile ->
    [ Producer $ saveProfile chan ks model ]

editProfileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> Keys
  -> ALens' sp EditProfileModel
  -> WidgetNode sp ep
editProfileWidget chan keys field = composite "editProfileWidget" field viewProfile (handleProfileEvent chan keys)

saveProfile :: TChan Request -> Keys -> EditProfileModel -> (ProfileEvent -> IO ()) -> IO ()
saveProfile chan (Keys kp xo _ _) model sendMsg = do
  now <- getCurrentTime
  let raw = setMetadata name about picture nip05 xo now
  atomically $ writeTChan chan $ SendEvent $ signEvent raw kp xo
  where
    is = model ^. inputs
    name = strip $ is ^. nameInput
    about = strip $ is ^. aboutInput
    picture = strip $ is ^. pictureUrlInput
    nip05 = strip $ is ^. nip05IdentifierInput

viewProfile :: WidgetEnv EditProfileModel ProfileEvent -> EditProfileModel -> WidgetNode EditProfileModel ProfileEvent
viewProfile wenv model =
  vstack
    [ label "Profile"
    , spacer
    , hstack
        [ label "Name"
        , filler
        , tf (inputs . nameInput) "nameInput"
        ]
    , spacer
    , hstack
        [ label "About"
        , filler
        , tf (inputs . aboutInput) "aboutInput"
        ]
    , spacer
    , hstack
        [ label "Picture URL"
        , filler
        , tf (inputs . pictureUrlInput) "pictureUrlInput"
        ]
    , spacer
    , hstack
        [ label "NIP-05 Identifier"
        , filler
        , tf (inputs . nip05IdentifierInput) "nip05IdentifierInput"
        ]
    , spacer
    , button "Save" SaveProfile
    ]
    where
      tf input id' = textField input `nodeKey` id' `styleBasic` [ width 400 ]
