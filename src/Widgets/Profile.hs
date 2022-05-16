{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Profile where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad.STM                    (atomically)
import           Crypto.Schnorr
import           Data.DateTime
import           Data.Default
import           Data.Text
import           Monomer

import           Helpers
import           NostrFunctions
import           NostrTypes

data ProfileModelInputs = ProfileModelInputs
  { _nameInput            :: Text
  , _aboutInput           :: Text
  , _pictureUrlInput      :: Text
  , _nip05IdentifierInput :: Text
  , _errors               :: [Text]
} deriving (Eq, Show)

instance Default ProfileModelInputs where
  def = ProfileModelInputs "" "" "" "" []

data ProfileModel =  ProfileModel
  { _name            :: Text
  , _about           :: Text
  , _pictureUrl      :: Text
  , _nip05Identifier :: Text
  , _inputs          :: ProfileModelInputs
  } deriving (Eq, Show)

instance Default ProfileModel where
  def = ProfileModel "" "" "" "" def


data ProfileEvent
  = SaveProfile
  deriving (Eq, Show)

makeLenses 'ProfileModelInputs
makeLenses 'ProfileModel

handleProfileEvent
  :: TChan ServerRequest
  -> Keys
  -> WidgetEnv ProfileModel ProfileEvent
  -> WidgetNode ProfileModel ProfileEvent
  -> ProfileModel
  -> ProfileEvent
  -> [EventResponse ProfileModel ProfileEvent sp ep]
handleProfileEvent chan ks env node model evt = case evt of
  SaveProfile ->
    [ Producer $ saveProfile chan ks model ]

profileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan ServerRequest
  -> Keys
  -> ALens' sp ProfileModel
  -> WidgetNode sp ep
profileWidget chan keys field = composite "profileWidget" field viewProfile (handleProfileEvent chan keys)

saveProfile :: TChan ServerRequest -> Keys -> ProfileModel -> (ProfileEvent -> IO ()) -> IO ()
saveProfile chan ks model sendMsg = do
  now <- getCurrentTime
  let raw = setMetadata name about picture nip05 xo now
  atomically $ writeTChan chan $ SendEvent $ signEvent raw (fst' ks) xo
  where
    is = model ^. inputs
    xo = snd' ks
    name = strip $ is ^. nameInput
    about = strip $ is ^. aboutInput
    picture = strip $ is ^. pictureUrlInput
    nip05 = strip $ is ^. nip05IdentifierInput

viewProfile :: WidgetEnv ProfileModel ProfileEvent -> ProfileModel -> WidgetNode ProfileModel ProfileEvent
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
    ] `styleBasic` [padding 10]
    where
      tf input id' = textField input `nodeKey` id' `styleBasic` [ width 400 ]
