{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module QtQuick where

import Control.Monad (forever, forM_, void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
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


type PropertyName = Text


data PropertyMap = PropertyMap
  { profileObjRefs :: Map PubKeyXO (Map PropertyName (QML.WeakObjRef PubKeyXO))
  , postObjRefs :: Map EventId (Map PropertyName (QML.WeakObjRef EventId))
  }


-- | UI updates
data UIUpdates = UIUpdates
  { myFollowsChanged :: Bool
  , postsChanged :: Bool
  , privateMessagesChanged :: Bool
  , dmRelaysChanged :: Bool
  , generalRelaysChanged :: Bool
  , tempRelaysChanged :: Bool
  , publishStatusChanged :: Bool
  , noticesChanged :: Bool
  , inboxModelStateChanged :: Bool
  , postObjectsToSignal :: [QML.ObjRef EventId]
  , profileObjectsToSignal :: [QML.ObjRef PubKeyXO]
  , generalObjectsToSignal :: [QML.ObjRef ()]
  }


instance Semigroup UIUpdates where
  a <> b = UIUpdates
    { myFollowsChanged = myFollowsChanged a || myFollowsChanged b
    , postsChanged = postsChanged a || postsChanged b
    , privateMessagesChanged = privateMessagesChanged a || privateMessagesChanged b
    , dmRelaysChanged = dmRelaysChanged a || dmRelaysChanged b
    , generalRelaysChanged = generalRelaysChanged a || generalRelaysChanged b
    , tempRelaysChanged = tempRelaysChanged a || tempRelaysChanged b
    , publishStatusChanged = publishStatusChanged a || publishStatusChanged b
    , noticesChanged = noticesChanged a || noticesChanged b
    , inboxModelStateChanged = inboxModelStateChanged a || inboxModelStateChanged b
    , postObjectsToSignal = postObjectsToSignal a ++ postObjectsToSignal b
    , profileObjectsToSignal = profileObjectsToSignal a ++ profileObjectsToSignal b
    , generalObjectsToSignal = generalObjectsToSignal a ++ generalObjectsToSignal b
    }


instance Monoid UIUpdates where
  mempty = emptyUpdates


-- | Empty UI updates.
emptyUpdates :: UIUpdates
emptyUpdates = UIUpdates
  False False False False False False False False False [] [] []


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
  SignalPost :: QML.ObjRef EventId -> QtQuick m ()
  SignalProfile :: QML.ObjRef PubKeyXO -> QtQuick m ()
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

          -- Handle boolean flag updates
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

          -- Handle specific object updates
          forM_ (postObjectsToSignal combinedUpdates) $ \objRef ->
            liftIO $ QML.fireSignal changeKey objRef

          forM_ (profileObjectsToSignal combinedUpdates) $ \objRef ->
            liftIO $ QML.fireSignal changeKey objRef

          forM_ (generalObjectsToSignal combinedUpdates) $ \objRef ->
            liftIO $ QML.fireSignal changeKey objRef

          threadDelay 1200000  -- 0.2 second delay for UI updates

        liftIO $ QML.runEngineLoop config

    CreateSignalKey -> liftIO $ QML.newSignalKey

    FireSignal objRef -> do
        st <- get
        case signalKey st of
            Just key -> liftIO $ QML.fireSignal key objRef
            Nothing -> logError "No signal key available"

    SignalPost objRef -> do
        st <- get
        case signalKey st of
            Just key -> liftIO $ QML.fireSignal key objRef
            Nothing -> logError "No signal key available"

    SignalProfile objRef -> do
        st <- get
        case signalKey st of
            Just key -> liftIO $ QML.fireSignal key objRef
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


-- | Check if the UI updates have any changes.
hasUpdates :: UIUpdates -> Bool
hasUpdates u =
    myFollowsChanged u ||
    postsChanged u ||
    privateMessagesChanged u ||
    dmRelaysChanged u ||
    generalRelaysChanged u ||
    tempRelaysChanged u ||
    publishStatusChanged u ||
    noticesChanged u ||
    inboxModelStateChanged u ||
    not (null (postObjectsToSignal u)) ||
    not (null (profileObjectsToSignal u)) ||
    not (null (generalObjectsToSignal u))
