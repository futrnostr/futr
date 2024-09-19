module Nostr.Effects.Dispatcher where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TQueue, atomically, writeTQueue)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get)
import Effectful.TH

import Nostr.Effects.Logging
import Nostr.Types

-- | Effect for dispatching responses to subscriptions.
data Dispatcher :: Effect where
    DispatchResponse :: Response -> Dispatcher m ()

makeEffect ''Dispatcher

-- | State for the dispatcher.
data DispatcherState = DispatcherState
  { subscriptions :: Map SubscriptionId (TQueue Response, Relay)
  }

-- | Effect for dispatching responses to subscripti ns.
type DispatcherEff es = (Logging :> es, Concurrent :> es)

-- | Initial state for the dispatcher.
initialState :: DispatcherState
initialState = DispatcherState { subscriptions = Map.empty }

-- | Run the dispatcher effect.
runDispatcher :: DispatcherEff es => Eff (Dispatcher : State DispatcherState : es) a -> Eff es a
runDispatcher action = evalState initialState $ interpret handleDispatcher action
  where
    handleDispatcher :: DispatcherEff es => EffectHandler Dispatcher (State DispatcherState : es)
    handleDispatcher _ = \case
      DispatchResponse response -> do
        case response of
          EventReceived subId' _ -> forwardResponse subId' response
          Ok eventId' accepted msg -> logInfo $ "Event " <> (T.pack $ show eventId') <> if accepted then " accepted" else " rejected" <> ": " <> msg
          Eose subId' -> forwardResponse subId' response
          Closed subId' _ -> forwardResponse subId' response
          Notice msg -> logWarning $ "Received notice: " <> msg

    forwardResponse :: DispatcherEff es => SubscriptionId -> Response -> Eff (State DispatcherState : es) ()
    forwardResponse subId' response = do
      st <- get
      case Map.lookup subId' (subscriptions st) of
        Just (queue, _) -> do
          logDebug $ "Forwarding response to subscription: " <> subId'
          logDebug $ "Response: " <> T.pack (show response)
          atomically $ writeTQueue queue response
        Nothing -> logWarning $ "Received response for unknown subscription: " <> subId'
