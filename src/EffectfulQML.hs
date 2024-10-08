{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EffectfulQML where

import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, put)
import Effectful.TH
import Graphics.QML qualified as QML

import Nostr.Effects.Logging

data EffectfulQMLState = EffectfulQMLState
  { signalKey :: Maybe (QML.SignalKey (IO ()))
  , rootObjRef :: Maybe (QML.ObjRef ())
  }

-- | Define the effects for QML operations.
data EffectfulQML :: Effect where
  RunEngineLoop :: QML.EngineConfig -> QML.SignalKey (IO ()) -> QML.ObjRef () -> EffectfulQML m ()
  CreateSignalKey :: EffectfulQML m (QML.SignalKey (IO ()))
  FireSignalWith :: QML.SignalKey (IO ()) -> QML.ObjRef () -> EffectfulQML m ()
  FireSignal :: QML.ObjRef () -> EffectfulQML m ()
  FireSignalOnRoot :: EffectfulQML m ()


type instance DispatchOf EffectfulQML = Dynamic

makeEffect ''EffectfulQML

-- | Handler for the QML effects.
runEffectfulQML :: (IOE :> es, Logging :> es) => Eff (EffectfulQML : State EffectfulQMLState : es) a -> Eff es a
runEffectfulQML action = evalState (EffectfulQMLState Nothing Nothing) $ interpret handleEffectfulQML action
  where
    handleEffectfulQML
      :: (IOE :> es, Logging :> es)
      => EffectHandler EffectfulQML (State EffectfulQMLState : es)
    handleEffectfulQML _ = \case
      RunEngineLoop config changeKey ctx -> do
        put $ EffectfulQMLState (Just changeKey) (Just ctx)
        liftIO $ QML.runEngineLoop config

      CreateSignalKey -> liftIO $ QML.newSignalKey

      FireSignalWith changeKey obj -> liftIO $ QML.fireSignal changeKey obj

      FireSignal obj -> do
        st <- get
        case signalKey st of
          Just key -> liftIO $ QML.fireSignal key obj
          Nothing -> error "No signal key available"

      FireSignalOnRoot -> do
        st <- get
        case (rootObjRef st, signalKey st) of
          (Just rootObj', Just changeKey') -> do
            logDebug "Firing signal on root"
            liftIO $ QML.fireSignal changeKey' rootObj'
          _ -> logWarning "No rootObjRef or signalKey in AppState"
