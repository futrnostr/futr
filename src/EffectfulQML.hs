{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EffectfulQML where

import Control.Monad (forM_, when)
import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, gets, put)
import Effectful.TH
import Graphics.QML qualified as QML

import Logging
import Types (AppState(..), UIReferences(..), UIUpdates(..))

data EffectfulQMLState = EffectfulQMLState
  { signalKey :: Maybe (QML.SignalKey (IO ()))
  , rootObjRef :: Maybe (QML.ObjRef ())
  }

-- | Define the effects for QML operations.
data EffectfulQML :: Effect where
  RunEngineLoop :: QML.EngineConfig -> QML.SignalKey (IO ()) -> QML.ObjRef () -> EffectfulQML m ()
  CreateSignalKey :: EffectfulQML m (QML.SignalKey (IO ()))
  FireSignal :: QML.ObjRef () -> EffectfulQML m ()
  -- object specific signals
  Notify :: UIUpdates -> EffectfulQML m ()
  NotifyRelayStatus :: EffectfulQML m ()


type instance DispatchOf EffectfulQML = Dynamic

makeEffect ''EffectfulQML

-- | Handler for the QML effects.
runEffectfulQML
  :: (IOE :> es, Logging :> es, State AppState :> es)
  => Eff (EffectfulQML : State EffectfulQMLState : es) a
  -> Eff es a
runEffectfulQML action = evalState (EffectfulQMLState Nothing Nothing) $ interpret handleEffectfulQML action
  where
    handleEffectfulQML
      :: (IOE :> es, Logging :> es, State AppState :> es)
      => EffectHandler EffectfulQML (State EffectfulQMLState : es)
    handleEffectfulQML _ = \case
      RunEngineLoop config changeKey ctx -> do
        put $ EffectfulQMLState (Just changeKey) (Just ctx)
        liftIO $ QML.runEngineLoop config

      CreateSignalKey -> liftIO $ QML.newSignalKey

      FireSignal obj -> do
        st <- get
        case signalKey st of
          Just key -> liftIO $ QML.fireSignal key obj
          Nothing -> logError "No signal key available"

      NotifyRelayStatus -> do
        st <- get
        refs <- gets @AppState uiRefs
        case signalKey st of
          Just key -> do
            forM_ (dmRelaysObjRef refs) (liftIO . QML.fireSignal key)
            forM_ (generalRelaysObjRef refs) (liftIO . QML.fireSignal key)
          Nothing -> logError "No signal key available"

      Notify u -> do
        st <- get
        refs <- gets @AppState uiRefs
        case signalKey st of
          Just key -> do
            when (profilesChanged u) $ forM_ (profileObjRef refs) (liftIO . QML.fireSignal key)
            when (followsChanged u) $ forM_ (followsObjRef refs) (liftIO . QML.fireSignal key)
            when (chatsChanged u) $ forM_ (chatObjRef refs) (liftIO . QML.fireSignal key)
            when (dmRelaysChanged u) $ forM_ (dmRelaysObjRef refs) (liftIO . QML.fireSignal key)
            when (generalRelaysChanged u) $ forM_ (generalRelaysObjRef refs) (liftIO . QML.fireSignal key)
            -- there is no obj ref for publish status
            --when (publishStatusChanged u) $ forM_ (publishStatusObjRef refs) (liftIO . QML.fireSignal key)
            -- there is no obj ref for notices
            --when (noticesChanged u) $ forM_ (noticesObjRef refs) (liftIO . QML.fireSignal key)
          Nothing -> logError "No signal key available"

