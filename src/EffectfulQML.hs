{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EffectfulQML where

import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, put)
import Effectful.TH
import Graphics.QML qualified as QML

data EffectfulQMLState = EffectfulQMLState
  { signalKey :: Maybe (QML.SignalKey (IO ()))
  }

-- | Define the effects for QML operations.
data EffectfulQML :: Effect where
  RunEngineLoop :: QML.EngineConfig -> QML.SignalKey (IO ()) -> EffectfulQML m ()
  CreateSignalKey :: EffectfulQML m (QML.SignalKey (IO ()))
  FireSignalWith :: QML.SignalKey (IO ()) -> QML.ObjRef () -> EffectfulQML m ()
  FireSignal :: QML.ObjRef () -> EffectfulQML m ()


type instance DispatchOf EffectfulQML = Dynamic

makeEffect ''EffectfulQML

-- | Handler for the QML effects.
runEffectfulQML :: (IOE :> es) => Eff (EffectfulQML : State EffectfulQMLState : es) a -> Eff es a
runEffectfulQML action = evalState (EffectfulQMLState Nothing) $ interpret handleEffectfulQML action
  where
    handleEffectfulQML
      :: (IOE :> es)
      => EffectHandler EffectfulQML (State EffectfulQMLState : es)
    handleEffectfulQML _ = \case
      RunEngineLoop config changeKey -> do
        put $ EffectfulQMLState $ Just changeKey
        liftIO $ QML.runEngineLoop config

      CreateSignalKey -> liftIO $ QML.newSignalKey

      FireSignalWith changeKey obj -> liftIO $ QML.fireSignal changeKey obj

      FireSignal obj -> do
        st <- get
        case signalKey st of
          Just s -> liftIO $ QML.fireSignal s obj
          Nothing -> return()
