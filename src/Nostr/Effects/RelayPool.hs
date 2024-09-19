{-# LANGUAGE BlockArguments #-}

module Nostr.Effects.RelayPool where

import Control.Monad (forM, forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TChan, atomically, newTChanIO, newTQueueIO, writeTChan)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, modify)
import Effectful.TH

import Nostr.Effects.Dispatcher hiding (initialState)
import Nostr.Effects.IDGen
import Nostr.Effects.Logging
import Nostr.Effects.WebSocket
import Nostr.Relay
import Nostr.Types

-- | Effect for handling RelayPool operations.
data RelayPool :: Effect where
    AddRelay :: Relay -> RelayPool m ()
    Connect :: Relay -> RelayPool m ()
    Disconnect :: Relay -> RelayPool m ()
    DisconnectAll :: RelayPool m ()
    SendEvent :: Event -> [Relay] -> RelayPool m ()
    GetRelays :: RelayPool m [(Relay, Bool)]
    StartSubscription :: [Filter] -> [Relay] -> RelayPool m [(SubscriptionId, Relay)]
    StopSubscription :: SubscriptionId -> RelayPool m ()

type instance DispatchOf RelayPool = Dynamic

makeEffect ''RelayPool

-- | State for RelayPool handling.
data RelayPoolState = RelayPoolState
    { relays        :: [(Relay, Bool)]
    , relayChannels :: Map RelayURI (TChan Request)
    }

initialState :: RelayPoolState
initialState = RelayPoolState
  { relays = []
  , relayChannels = Map.empty
  }

type RelayPoolEff es = (WebSocket :> es, State DispatcherState :> es, Concurrent :> es, Logging :> es, IDGen :> es)

-- | Handler for relay pool effects.
runRelayPool
  :: RelayPoolEff es
  => Eff (RelayPool : State RelayPoolState : es) a
  -> Eff es a
runRelayPool action = evalState initialState $ interpret handleRelayPool action
  where
    handleRelayPool :: RelayPoolEff es => EffectHandler RelayPool (State RelayPoolState : es)
    handleRelayPool _ = \case
      AddRelay relay -> do
        st <- get
        let existingRelays = relays st
        if any (\(r, _) -> r `sameRelay` relay) existingRelays
        then return ()
        else do
          newChan <- newTChanIO
          let updatedRelays = (relay, False) : existingRelays
              updatedChannels = Map.insert (uri relay) newChan (relayChannels st)
          modify $ \st' -> st' { relays = updatedRelays, relayChannels = updatedChannels }

      Connect relay -> do
        modify $ \st -> st { relays = map (\(r, c) -> if r `sameRelay` relay then (r, True) else (r, c)) (relays st) }
        st <- get
        case Map.lookup (uri relay) (relayChannels st) of
          Just chan -> runClient relay chan
          Nothing -> logError $ "Cannot connect to unknown relay: " <> relayName relay <> ", maybe you forgot to call AddRelay?"

      Nostr.Effects.RelayPool.Disconnect relay -> do
        logDebug $ T.pack $ "Disconnecting from " ++ T.unpack (relayName relay) ++ " ..."
        modify $ \st -> st { relays = map (\(r, c) -> if uri r == uri relay then (r, False) else (r, c)) (relays st) }
        st <- get
        case Map.lookup (uri relay) (relayChannels st) of
          Just chan -> atomically $ writeTChan chan Nostr.Types.Disconnect
          Nothing -> return ()

      Nostr.Effects.RelayPool.DisconnectAll -> do
        logDebug $ "Disconnecting from all relays ..."
        st <- get
        let updatedRelays = map (\(r, _) -> (r, False)) (relays st)
        modify $ \st' -> st' { relays = updatedRelays }
        forM_ (Map.elems $ relayChannels st) $ \chan -> atomically $ writeTChan chan Nostr.Types.Disconnect

      Nostr.Effects.RelayPool.SendEvent event rs -> do
        st <- get
        let channels = relayChannels st
        forM_ rs $ \relay -> do
          case Map.lookup (uri relay) channels of
            Just chan -> do
              atomically $ writeTChan chan (Nostr.Types.SendEvent event)
              logDebug $ "Sent event: " <> T.pack (show event) <> " to relay: " <> relayName relay
            Nothing -> logWarning $ "No channel found for relay: " <> relayName relay

      GetRelays -> do
        st <- get
        return $ relays st

      StartSubscription filters' relays' -> do
        st <- get
        subscriptionIds <- forM relays' $ \relay -> do
          subId' <- generateID 8
          case Map.lookup (uri relay) (relayChannels st) of
            Just chan -> do
              queue <- newTQueueIO
              modify $ \st' -> st' { subscriptions = Map.insert subId' (queue, relay) (subscriptions st') }
              atomically $ writeTChan chan (Subscribe $ Subscription filters' subId')
              logDebug $ "Starting new subscription: " <> subId' <> " on relay: " <> relayName relay
              return (subId', relay)
            Nothing -> do
              logError $ "No channel found for relay: " <> relayName relay
              return (subId', relay)
        return subscriptionIds

      StopSubscription subId' -> do
        dst <- get @DispatcherState
        case Map.lookup subId' (subscriptions dst) of
          Just (_, relay) -> do
            st <- get
            case Map.lookup (uri relay) (relayChannels st) of
              Just chan -> do
                atomically $ writeTChan chan (Close subId')
                logDebug $ "Closed subscription: " <> subId' <> " on relay: " <> relayName relay
              Nothing -> logWarning $ "No channel found for relay: " <> relayName relay
            modify $ \st' -> st' { subscriptions = Map.delete subId' (subscriptions st') }
          Nothing -> return ()
