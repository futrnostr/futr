{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Setup where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Crypto.Schnorr
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.Map                     (Map)
import Data.Maybe
import Data.Text                    (Text, pack)
import Monomer

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map             as Map

import Helpers
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response
import UIHelpers
import Widgets.ProfileImage

type SetupWenv = WidgetEnv SetupModel SetupEvent

type SetupNode = WidgetNode SetupModel SetupEvent

data SetupModel = SetupModel
  { _secretKeyInput :: Text
  , _keys           :: Keys
  , _name           :: Username
  , _displayName    :: DisplayName
  , _about          :: About
  , _picture        :: Picture
  , _currentImage   :: Picture
  , _imported       :: Bool
  } deriving (Eq, Show)

instance Default SetupModel where
  def = SetupModel "" initialKeys "" "" "" "" "" False

data SetupEvent
  = ImportSecKey
  | SecKeyImported Keys Profile DateTime
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | CreateAccount Keys Profile
  | ImportAccount Keys Profile
  | LoadImage
  | SetupDone Keys Profile DateTime
  deriving Show

makeLenses 'SetupModel

setupWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> MVar RelayPool
  -> (Keys -> Profile -> DateTime -> ep)
  -> ALens' sp SetupModel
  -> WidgetNode sp ep
setupWidget requestChannel poolMVar reportKeys model =
  composite_
    "SetupWidget"
    model
    buildUI
    (handleSetupEvent requestChannel poolMVar reportKeys)
    [ onInit GenerateKeyPair ]

handleSetupEvent
  :: (WidgetEvent ep)
  => TChan Request
  -> MVar RelayPool
  -> (Keys -> Profile -> DateTime -> ep)
  -> SetupWenv
  -> SetupNode
  -> SetupModel
  -> SetupEvent
  -> [EventResponse SetupModel SetupEvent sp ep]
handleSetupEvent requestChannel poolMVar reportKeys env node model evt = case evt of
  ImportSecKey ->
    [ Model $ model
      & keys .~ ks
      & imported .~ True
    , Task $ loadImportedKeyData requestChannel poolMVar ks SecKeyImported
    ]
    where
      kp =
        fromJust $
        fmap keyPairFromSecKey $
        maybe Nothing secKey $ decodeHex $ model ^. secretKeyInput
      xo = deriveXOnlyPubKey $ kp
      ks = Keys kp xo True Nothing
  SecKeyImported keys (Profile username' displayName' about' picture') _ ->
    [ Model $ model
        & name .~ username'
        & displayName .~ fromMaybe "" displayName'
        & about .~ fromMaybe "" about'
        & picture .~ fromMaybe "" picture'
    ]
  GenerateKeyPair ->
    [ Task generateNewKeyPair
    , Model $ model
        & imported .~ False
        & secretKeyInput .~ ""
    ]
  KeyPairGenerated kp ->
    [ Model $ model & keys .~ ks
    ]
    where
      xo = deriveXOnlyPubKey kp
      ks = Keys kp xo True Nothing
  CreateAccount ks profile ->
    [ Task $ createAccount requestChannel ks profile ]
  ImportAccount ks profile ->
    [ Task $ importAccount requestChannel ks profile ]
  LoadImage ->
    [ Model $ model & currentImage .~ model ^. picture ]
  SetupDone ks profile datetime ->
    [ Report $ reportKeys ks profile datetime ]

buildUI :: SetupWenv -> SetupModel -> SetupNode
buildUI wenv model = setupView where
  textToMaybe f = if "" == f
    then Nothing
    else Just $ f
  profile = Profile
    (model ^. name)
    (textToMaybe $ model ^. displayName)
    (textToMaybe $ model ^. about)
    (textToMaybe $ model ^. picture)
  ks = model ^. keys
  formLabel t = label t `styleBasic` [ width 150 ]
  myProfileImage = case model ^. currentImage of
    "" ->
      profileImage_ Nothing xo [ fitEither ] `styleBasic` [ width 300, height 300 ]
    pi ->
      profileImage_ (Just $ model ^. currentImage) xo [ fitEither ] `styleBasic` [ width 300, height 300 ]
  info = case model ^. currentImage of
    "" ->
      label "Robots lovingly delivered by Robohash.org" `styleBasic` [ textSize 8 ]
    _ ->
      hstack []
  form = vstack
    [ vstack
        [ bigLabel "Create Account"
        , spacer
        , hstack [ formLabel "Username", textField name `nodeKey` "username" ]
        , spacer
        , hstack [ formLabel "Display name", textField displayName `nodeKey` "displayName" ]
        , spacer
        , hstack [ formLabel "About", textField about `nodeKey` "about" ]
        , spacer
        , hstack [ formLabel "Picture URL", textField picture `nodeKey` "picture" ]
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
            , case model ^. imported of
                True ->
                  mainButton "Import Account" (ImportAccount ks profile)
                    `nodeEnabled` (model ^. name /= "")
                False ->
                  mainButton "Create Account" (CreateAccount ks profile)
                    `nodeEnabled` (model ^. name /= "")
            ]
        , spacer
        , label "Public Key"
        , spacer
        , label (pack $ exportXOnlyPubKey xo) `styleBasic` [ textSize 11 ]
        , spacer
        , label "Private Key"
        , spacer
        , label (pack $ (exportSecKey . deriveSecKey) kp) `styleBasic` [ textSize 11 ]
    ] `styleBasic` [ paddingL 20 ]
  setupView = vstack
    [ hstack
        [ vstack
            [ myProfileImage
            , spacer
            , info
            , spacer
            , button "Generate new key pair" GenerateKeyPair
            ]
        , form
        ]
    , filler
    , separatorLine
    , filler
    , label "Import an existing private key (32 byte hex-encoded)"
    , spacer
    , hstack
        [ textField secretKeyInput `nodeKey` "importPrivateKey"
        , spacer
        , button "Import" ImportSecKey `nodeEnabled` isValidPrivateKey
        ]
    ] `styleBasic` [ padding 10 ]
  Keys kp xo _ _ = model ^. keys
  isValidPrivateKey =
    isJust $ maybe Nothing secKey $ decodeHex $ view secretKeyInput model

generateNewKeyPair :: IO SetupEvent
generateNewKeyPair = do
  kp <- generateKeyPair
  return $ KeyPairGenerated kp

createAccount :: TChan Request -> Keys -> Profile -> IO SetupEvent
createAccount requestChannel keys profile = do
  let (Keys kp xo _ _) = keys
  let (Profile name _ _ _) = profile
  now <- getCurrentTime
  send requestChannel $ SendEvent $ signEvent (setMetadata profile xo now) kp xo
  send requestChannel $ SendEvent $ signEvent (setContacts [(xo, Just name)] xo (addSeconds 1 now)) kp xo
  return $ SetupDone (Keys kp xo True (Just name)) profile now

importAccount :: TChan Request -> Keys -> Profile -> IO SetupEvent
importAccount requestChannel keys profile = do
  let (Keys kp xo _ _) = keys
  let (Profile name _ _ _) = profile
  now <- getCurrentTime
  send requestChannel $ SendEvent $ signEvent (setMetadata profile xo now) kp xo
  return $ SetupDone (Keys kp xo True (Just name)) profile now
