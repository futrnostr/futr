{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EffectfulQML where

import Control.Monad (forever, forM_, void, when)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO, readTQueue, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, put)
import Effectful.TH
import Graphics.QML qualified as QML

import Logging
import Types (AppState(..), UIReferences(..), UIUpdates(..), emptyUpdates)


-- | Effectful QML state.
data EffectfulQMLState = EffectfulQMLState
  { signalKey :: Maybe (QML.SignalKey (IO ()))
  , rootObjRef :: Maybe (QML.ObjRef ())
  , uiRefs :: UIReferences
  , queue :: Maybe (TQueue UIUpdates)
  }


-- | Initial effectful QML state.
initialEffectfulQMLState :: EffectfulQMLState
initialEffectfulQMLState = EffectfulQMLState Nothing Nothing initialUIRefs Nothing


-- | Initial UI references.
initialUIRefs :: UIReferences
initialUIRefs = UIReferences Nothing Nothing Nothing Nothing Nothing Nothing Nothing


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
  :: (IOE :> es, Concurrent :> es, Logging :> es, State EffectfulQMLState :> es, State AppState :> es)
  => Eff (EffectfulQML : es) a
  -> Eff es a
runEffectfulQML = interpret $ \_ -> \case
    RunEngineLoop config changeKey ctx -> do
        q <- newTQueueIO
        put $ EffectfulQMLState (Just changeKey) (Just ctx) initialUIRefs (Just q)
        void $ async $ forever $ do
          uiUpdates <- atomically $ readTQueue q
          moreUpdates <- atomically $ flushTQueue q
          let combinedUpdates = uiUpdates <> mconcat moreUpdates

          refs <- gets uiRefs
          -- Define update checks and their corresponding refs
          let updates = [ (profilesChanged, profileObjRef)
                        , (followsChanged, followsObjRef)
                        , (postsChanged, postsObjRef)
                        , (privateMessagesChanged, privateMessagesObjRef)
                        , (dmRelaysChanged, dmRelaysObjRef)
                        , (generalRelaysChanged, generalRelaysObjRef)
                        , (tempRelaysChanged, tempRelaysObjRef)
                        ]

          forM_ updates $ \(checkFn, getRef) ->
            when (checkFn combinedUpdates) $
              forM_ (getRef refs) (liftIO . QML.fireSignal changeKey)

          threadDelay 200000  -- 0.2 second delay for UI updates

        liftIO $ QML.runEngineLoop config

    CreateSignalKey -> liftIO $ QML.newSignalKey

    FireSignal obj -> do
        st <- get
        case signalKey st of
          Just key -> liftIO $ QML.fireSignal key obj
          Nothing -> logError "No signal key available"

    NotifyRelayStatus -> do
        st <- get
        case queue st of
          Just q -> do
            let updates = emptyUpdates { dmRelaysChanged = True, generalRelaysChanged = True, tempRelaysChanged = True }
            atomically $ writeTQueue q updates
          Nothing -> logError "No queue available"

    Notify u -> do
        st <- get
        case queue st of
          Just q -> atomically $ writeTQueue q u
          Nothing -> logError "No queue available"
