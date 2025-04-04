{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module QtQuick where

import Control.Monad (forever, forM_, void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO, readTQueue, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, put)
import Effectful.TH
import Graphics.QML qualified as QML

import Logging
import Nostr.Event (EventId)
import Nostr.Keys (PubKeyXO)
import Types (AppState(..))


-- | Effectful QML state.
data QtQuickState = QtQuickState
  { signalKey :: Maybe (QML.SignalKey (IO ()))
  , rootObjRef :: Maybe (QML.ObjRef ())
  , uiRefs :: UIReferences
  , propertyMap :: PropertyMap
  , queue :: Maybe (TQueue UIUpdates)
  }


-- | UI object references grouped together
data UIReferences = UIReferences
  { followsObjRef :: Maybe (QML.ObjRef ())
  , postsObjRef :: Maybe (QML.ObjRef ())
  , currentPostCommentsObjRef :: Maybe (QML.ObjRef EventId)
  , privateMessagesObjRef :: Maybe (QML.ObjRef ())
  , dmRelaysObjRef :: Maybe (QML.ObjRef ())
  , generalRelaysObjRef :: Maybe (QML.ObjRef ())
  , tempRelaysObjRef :: Maybe (QML.ObjRef ())
  , publishStatusObjRef :: Maybe (QML.ObjRef ())
  , inboxModelStateObjRef :: Maybe (QML.ObjRef ())
  }


data PropertyMap = PropertyMap
  { profileObjRefs :: Map PubKeyXO [QML.WeakObjRef PubKeyXO]
  , postObjRefs :: Map EventId [QML.WeakObjRef EventId]
  }


-- | UI updates
data UIUpdates = UIUpdates
  { profilesChanged :: Bool
  , followsChanged :: Bool
  , myFollowsChanged :: Bool
  , postsChanged :: Bool
  , privateMessagesChanged :: Bool
  , dmRelaysChanged :: Bool
  , generalRelaysChanged :: Bool
  , tempRelaysChanged :: Bool
  , publishStatusChanged :: Bool
  , noticesChanged :: Bool
  , inboxModelStateChanged :: Bool
  } deriving (Eq, Show)


instance Semigroup UIUpdates where
  a <> b = UIUpdates
    { profilesChanged = profilesChanged a || profilesChanged b
    , followsChanged = followsChanged a || followsChanged b
    , myFollowsChanged = myFollowsChanged a || myFollowsChanged b
    , postsChanged = postsChanged a || postsChanged b
    , privateMessagesChanged = privateMessagesChanged a || privateMessagesChanged b
    , dmRelaysChanged = dmRelaysChanged a || dmRelaysChanged b
    , generalRelaysChanged = generalRelaysChanged a || generalRelaysChanged b
    , tempRelaysChanged = tempRelaysChanged a || tempRelaysChanged b
    , publishStatusChanged = publishStatusChanged a || publishStatusChanged b
    , noticesChanged = noticesChanged a || noticesChanged b
    , inboxModelStateChanged = inboxModelStateChanged a || inboxModelStateChanged b
    }


instance Monoid UIUpdates where
  mempty = emptyUpdates


-- | Empty UI updates.
emptyUpdates :: UIUpdates
emptyUpdates = UIUpdates False False False False False False False False False False False


-- | Initial effectful QML state.
initialQtQuickState :: QtQuickState
initialQtQuickState = QtQuickState Nothing Nothing initialUIRefs (PropertyMap Map.empty Map.empty) Nothing


-- | Initial UI references.
initialUIRefs :: UIReferences
initialUIRefs = UIReferences Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- | Define the effects for QML operations.
data QtQuick :: Effect where
  RunEngineLoop :: QML.EngineConfig -> QML.SignalKey (IO ()) -> QML.ObjRef () -> QtQuick m ()
  CreateSignalKey :: QtQuick m (QML.SignalKey (IO ()))
  FireSignal :: QML.ObjRef () -> QtQuick m ()
  -- object specific signals
  Notify :: UIUpdates -> QtQuick m ()
  NotifyRelayStatus :: QtQuick m ()


type instance DispatchOf QtQuick = Dynamic

makeEffect ''QtQuick


-- | Handler for the QML effects.
runQtQuick
  :: (IOE :> es, Concurrent :> es, Logging :> es, State QtQuickState :> es, State AppState :> es)
  => Eff (QtQuick : es) a
  -> Eff es a
runQtQuick = interpret $ \_ -> \case
    RunEngineLoop config changeKey ctx -> do
        q <- newTQueueIO
        put $ QtQuickState (Just changeKey) (Just ctx) initialUIRefs (PropertyMap Map.empty Map.empty) (Just q)
        void $ async $ forever $ do
          uiUpdates <- atomically $ readTQueue q
          moreUpdates <- atomically $ flushTQueue q
          let combinedUpdates = uiUpdates <> mconcat moreUpdates
          refs <- gets uiRefs
          let updates = [ (myFollowsChanged, followsObjRef)
                        , (postsChanged, postsObjRef)
                        , (privateMessagesChanged, privateMessagesObjRef)
                        , (dmRelaysChanged, dmRelaysObjRef)
                        , (generalRelaysChanged, generalRelaysObjRef)
                        , (tempRelaysChanged, tempRelaysObjRef)
                        , (inboxModelStateChanged, inboxModelStateObjRef)
                        , (publishStatusChanged, publishStatusObjRef)
                        ]

          forM_ updates $ \(checkFn, getRef) ->
            when (checkFn combinedUpdates) $
              forM_ (getRef refs) (liftIO . QML.fireSignal changeKey)

          when (profilesChanged combinedUpdates) $ do
            pmap <- gets @QtQuickState propertyMap
            let allWeakRefs = concat $ Map.elems $ profileObjRefs pmap
            forM_ allWeakRefs $ \weakRef -> do
              objRef <- liftIO $ QML.fromWeakObjRef weakRef
              liftIO $ QML.fireSignal changeKey objRef
              -- case objRef of
              --   Just obj -> liftIO $ QML.fireSignal changeKey obj
              --   Nothing -> logError "No object reference available"

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
