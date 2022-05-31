{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Setup where

import Control.Lens
import Crypto.Schnorr
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Text      (Text)
import Monomer

import qualified Data.ByteString.Lazy as LazyBytes

import Nostr.Keys

type SetupWenv = WidgetEnv SetupModel SetupEvent

type SetupNode = WidgetNode SetupModel SetupEvent

data SetupModel = SetupModel
  { _secretKeyInput :: Text
  , _allKeys        :: [Keys]
  } deriving (Eq, Show)

instance Default SetupModel where
  def = SetupModel "" []

data SetupEvent
  = ImportSecKey
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  deriving (Eq, Show)

makeLenses 'SetupModel

handleSetupEvent
  :: (WidgetEvent ep)
  => ([Keys] -> ep)
  -> SetupWenv
  -> SetupNode
  -> SetupModel
  -> SetupEvent
  -> [EventResponse SetupModel SetupEvent sp ep]
handleSetupEvent reportKeys env node model evt = case evt of
  ImportSecKey ->
    [ Model $ model
      & allKeys .~ ks : dk
      & secretKeyInput .~ ""
    , Report $ reportKeys $ model ^. allKeys
    ]
    where
      kp =
        fromJust $
        fmap keyPairFromSecKey $
        maybe Nothing secKey $ decodeHex $ model ^. secretKeyInput
      xo = deriveXOnlyPubKey $ kp
      ks = Keys kp xo True Nothing
      dk = disableKeys $ model ^. allKeys
  GenerateKeyPair ->
    [ Task generateNewKeyPair ]
  KeyPairGenerated k ->
    [ Model $ model
      & allKeys .~ ks : dk
    , Report $ reportKeys $ model ^. allKeys
    ]
    where
      xo = deriveXOnlyPubKey k
      ks = Keys k xo True Nothing
      dk = disableKeys $ model ^. allKeys

setupWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => ([Keys] -> ep)
  -> ALens' sp SetupModel
  -> WidgetNode sp ep
setupWidget reportKeys model =
  composite
    "ViewSetupWidget"
    model
    viewSetup
    (handleSetupEvent reportKeys)

viewSetup :: SetupWenv -> SetupModel -> SetupNode
viewSetup wenv model =
  vstack
    [ label "Setup a new key pair"
    , spacer
    , hstack
        [ label "Generate new key pair"
        , spacer
        , button "Generate" GenerateKeyPair
        ]
    , spacer
    , label "or import an existing private key"
    , spacer
    , hstack
        [ textField secretKeyInput `nodeKey` "importmyprivatekey"
        , spacer
        , button "Import" ImportSecKey `nodeEnabled` isValidPrivateKey
        ]
    ] `styleBasic` [ padding 10 ]
  where
    isValidPrivateKey =
      isJust $ maybe Nothing secKey $ decodeHex $ view secretKeyInput model

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\(Keys kp xo _ n) -> Keys kp xo False n) ks

generateNewKeyPair :: IO SetupEvent
generateNewKeyPair = do
  kp <- generateKeyPair
  return $ KeyPairGenerated kp
