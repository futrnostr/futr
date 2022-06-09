{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Setup where

import Control.Concurrent.MVar
import Control.Monad.STM            (atomically)
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

import Nostr.Event
import Nostr.Keys
import Nostr.Kind
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
  , _name           :: Username
  , _displayName    :: DisplayName
  , _about          :: About
  , _picture        :: Picture
  , _imported       :: Bool
  } deriving (Eq, Show)

instance Default SetupModel where
  def = SetupModel "" Nothing [] Nothing "" "" "" "" False

data SetupEvent
  = ImportSecKey
  | SecKeyImported Keys MetadataContent
  | GenerateKeyPair
  | KeyPairGenerated KeyPair
  | CreateAccount Keys MetadataContent
  | AccountCreated Keys
  | ImportAccount Keys MetadataContent
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
      & keys .~ Just ks
      & imported .~ True
    , Task $ loadImportedKeyData requestChannel poolMVar ks
    ]
    where
      kp =
        fromJust $
        fmap keyPairFromSecKey $
        maybe Nothing secKey $ decodeHex $ model ^. secretKeyInput
      xo = deriveXOnlyPubKey $ kp
      ks = Keys kp xo True Nothing
  SecKeyImported keys (MetadataContent username' displayName' about' picture') ->
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
  KeyPairGenerated k ->
    [ Model $ model & keys .~ Just ks
    --, Report $ reportKeys ks
    ]
    where
      xo = deriveXOnlyPubKey k
      ks = Keys k xo True Nothing
  CreateAccount ks metadataContent ->
    [ Task $ createAccount requestChannel ks metadataContent
    ]
  AccountCreated ks ->
    [ Report $ reportKeys ks ]

viewSetup :: SetupWenv -> SetupModel -> SetupNode
viewSetup wenv model = setupView where
  metadataContent = MetadataContent
    (model ^. name)
    Nothing
    Nothing
    Nothing
  ks = fromJust $ model ^. keys
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
        , hstack [ formLabel "Picture URL", textField picture `nodeKey` "picture" ]
        ]
        , filler
        , hstack
            [ filler
            , case model ^. imported of
                True ->
                  mainButton "Import Account" (ImportAccount ks metadataContent)
                    `nodeEnabled` (model ^. name /= "")
                False ->
                  mainButton "Create Account" (CreateAccount ks metadataContent)
                    `nodeEnabled` (model ^. name /= "")
            ]
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
            [ fallbackProfileImage xo Big
            , spacer
            , label "Robots lovingly delivered by Robohash.org" `styleBasic` [ textSize 8 ]
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
  pk = maybe Nothing (\(Keys pk _ _ _) -> Just pk) (model ^. keys)
  xo = maybe Nothing (\(Keys _ xo _ _) -> Just xo) (model ^. keys)
  isValidPrivateKey =
    isJust $ maybe Nothing secKey $ decodeHex $ view secretKeyInput model

generateNewKeyPair :: IO SetupEvent
generateNewKeyPair = do
  kp <- generateKeyPair
  return $ KeyPairGenerated kp

loadImportedKeyData :: TChan Request -> MVar RelayPool -> Keys -> IO SetupEvent
loadImportedKeyData requestChannel poolMVar keys = do
  let (Keys pk xo _ _) = keys
  pool <- readMVar poolMVar
  responseChannel <- atomically newTChan
  (subId, pool) <- subscribe pool requestChannel [] responseChannel
  response <- atomically $ readTChan responseChannel
  case response of
    (EventReceived _ event) ->
      case kind event of
        Metadata -> do
          pool' <- unsubscribe pool requestChannel subId
          putMVar poolMVar pool'
          case readMetadataContent event of
            Just md ->
              return $ SecKeyImported keys md
            Nothing ->
              error "Unexpected event metadata received"
        _ -> error "Unexpected event kind received when loading key data"
    _ ->
      error "Unexpected response received when loading key data"

createAccount :: TChan Request -> Keys -> MetadataContent -> IO SetupEvent
createAccount requestChannel keys metadataContent = do
  let (Keys pk xo _ _) = keys
  let (MetadataContent name _ _ _) = metadataContent
  now <- getCurrentTime
  send requestChannel $ SendEvent $ signEvent (setMetadata metadataContent xo now) pk xo
  send requestChannel $ SendEvent $ signEvent (setContacts [(xo, Just name)] xo (addSeconds 1 now)) pk xo
  return $ AccountCreated $ Keys pk xo True (Just name)
