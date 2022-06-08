{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Setup where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Crypto.Schnorr
import Data.Aeson
import Data.Default
import Data.Map       (Map)
import Data.Maybe
import Data.Text      (Text, pack)
import Monomer

import qualified Data.ByteString.Lazy as LazyBytes

import Nostr.Keys
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response
import UIHelpers

type SetupWenv = WidgetEnv SetupModel SetupEvent

type SetupNode = WidgetNode SetupModel SetupEvent

data SetupModel = SetupModel
  { _secretKeyInput :: Text
  , _mainRelay      :: Maybe Relay
  , _relays         :: [Relay]
  , _keys           :: Maybe Keys
  , _name           :: Text
  , _displayName    :: Text
  , _about          :: Text
  , _picture        :: Text
  } deriving (Eq, Show)

instance Default SetupModel where
  def = SetupModel "" Nothing [] Nothing "" "" "" ""

data SetupEvent
  = ImportSecKey
  | SecKeyImported
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  deriving (Eq, Show)

makeLenses 'SetupModel

setupWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> MVar RelayPool
  -> (Keys -> ep)
  -> ALens' sp SetupModel
  -> WidgetNode sp ep
setupWidget requestChannel poolMVar reportKeys model =
  composite_
    "SetupWidget"
    model
    viewSetup
    (handleSetupEvent requestChannel poolMVar reportKeys)
    [ onInit GenerateKeyPair ]

handleSetupEvent
  :: (WidgetEvent ep)
  => TChan Request
  -> MVar RelayPool
  -> (Keys -> ep)
  -> SetupWenv
  -> SetupNode
  -> SetupModel
  -> SetupEvent
  -> [EventResponse SetupModel SetupEvent sp ep]
handleSetupEvent requestChannel poolMVar reportKeys env node model evt = case evt of
  ImportSecKey ->
    [ Model $ model
      & secretKeyInput .~ ""
      & keys .~ Just ks
    , Task $ loadImportedKeyData poolMVar ks
    ]
    where
      kp =
        fromJust $
        fmap keyPairFromSecKey $
        maybe Nothing secKey $ decodeHex $ model ^. secretKeyInput
      xo = deriveXOnlyPubKey $ kp
      ks = Keys kp xo True Nothing
  GenerateKeyPair ->
    [ Task generateNewKeyPair ]
  KeyPairGenerated k ->
    [ Model $ model & keys .~ Just ks
    --, Report $ reportKeys ks
    ]
    where
      xo = deriveXOnlyPubKey k
      ks = Keys k xo True Nothing

viewSetup :: SetupWenv -> SetupModel -> SetupNode
viewSetup wenv model = setupView where
  formLabel t = label t `styleBasic` [ width 150 ]
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
        , hstack [ formLabel "Picture", textField picture `nodeKey` "picture" ]
        ]
        , filler
        , hstack [ filler, mainButton "Create Account" SecKeyImported ]
        , spacer
        , label "Private Key"
        , spacer
        , label (pack $ maybe "" exportXOnlyPubKey xo) `styleBasic` [ textSize 11 ]
        , spacer
        , label "Public Key"
        , spacer
        , label (pack $ maybe "" (exportSecKey . deriveSecKey) pk) `styleBasic` [ textSize 11 ]
    ] `styleBasic` [ paddingL 20 ]
  setupView = vstack
    [ hstack
        [ vstack
            [ fallbackProfileImage xo Big, spacer, button "Generate new key pair" GenerateKeyPair ]
        , form
        ]
    , filler
    , separatorLine
    , filler
    , label "Import an existing private key (32 byte hex-encoded)"
    , spacer
    , hstack
        [ textField secretKeyInput `nodeKey` "importmyprivatekey"
        , spacer
        , button "Import" ImportSecKey `nodeEnabled` isValidPrivateKey
        ]
    ] `styleBasic` [ padding 10 ]
  pk = maybe Nothing (\(Keys pk _ _ _) -> Just pk) (model ^. keys)
  xo = maybe Nothing (\(Keys _ xo _ _) -> Just xo) (model ^. keys)
  isValidPrivateKey =
    isJust $ maybe Nothing secKey $ decodeHex $ view secretKeyInput model

generateNewKeyPair :: IO SetupEvent
generateNewKeyPair = do
  kp <- generateKeyPair
  return $ KeyPairGenerated kp

loadImportedKeyData :: MVar RelayPool -> Keys -> IO SetupEvent
loadImportedKeyData poolMVar (Keys pk xo _ _) = do
  (RelayPool _ handlers) <- readMVar poolMVar
  return SecKeyImported
