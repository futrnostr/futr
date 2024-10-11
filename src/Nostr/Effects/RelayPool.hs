{-# LANGUAGE BlockArguments #-}

module Nostr.Effects.RelayPool where

import Control.Monad (forM, forM_, unless)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TQueue, atomically, newTChanIO, newTQueueIO, writeTChan)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, modify)
import Effectful.TH

import Nostr.Effects.IDGen
import Nostr.Effects.Logging
import Nostr.Effects.WebSocket
import Nostr.Types
import Types (RelayPoolState(..), RelayData(..), initialRelayPoolState)

-- | Effect for handling RelayPool operations.
data RelayPool :: Effect where
    AddRelay :: Relay -> RelayPool m ()
    Connect :: Relay -> RelayPool m Bool
    Disconnect :: RelayURI -> RelayPool m ()
    DisconnectAll :: RelayPool m ()
    SendEvent :: Event -> [RelayURI] -> RelayPool m ()
    GetRelays :: RelayPool m [(Relay, ConnectionState)]
    StartSubscription :: RelayURI -> [Filter] -> RelayPool m (Maybe (SubscriptionId, TQueue Response))
    StopSubscription :: SubscriptionId -> RelayPool m ()
    UnsubscribeAllSubscriptionsFromRelay :: RelayURI -> RelayPool m ()

type instance DispatchOf RelayPool = Dynamic

makeEffect ''RelayPool

type RelayPoolEff es = (WebSocket :> es, State WebSocketState :> es, Concurrent :> es, Logging :> es, IDGen :> es)

data RelayPoolError = RelayNotFound RelayURI
  deriving (Show, Eq)

-- | Handler for relay pool effects.
runRelayPool
  :: RelayPoolEff es
  => Eff (RelayPool : State RelayPoolState : es) a
  -> Eff es a
runRelayPool action = evalState initialRelayPoolState $ interpret handleRelayPool action
  where
    handleRelayPool :: RelayPoolEff es => EffectHandler RelayPool (State RelayPoolState : es)
    handleRelayPool _ = \case
      AddRelay relay -> do
        st <- get @RelayPoolState
        let relayURI = uri relay
            existingRelays = relays st
        unless (Map.member relayURI existingRelays) do
          reqChan <- newTChanIO
          resQueue <- newTQueueIO
          let newRelayData = RelayData (info relay) reqChan resQueue [] []
          modify @RelayPoolState $ \st' ->
            st' { relays = Map.insert relayURI newRelayData (relays st') }
          modify @WebSocketState $ \wsState ->
            wsState { connections = Map.insert relayURI (RelayConnectionState Disconnected 0) (connections wsState) }

      Connect relay -> do
        let relayURI = uri relay
        st <- get @RelayPoolState
        case Map.lookup relayURI (relays st) of
          Just relayData -> do
            wsState <- get @WebSocketState
            case Map.lookup relayURI (connections wsState) of
              Just relayConnState ->
                case connectionStatus relayConnState of
                  Connected -> do 
                    logDebug $ "Already connected to " <> relayURIToText relayURI
                    return True
                  Disconnected -> do
                    result <- runClient relay (requestChannel relayData) (responseQueue relayData)
                    case result of
                      Left e -> do
                        logError $ "Failed to connect to " <> relayURIToText relayURI <> ": " <> T.pack (show e)
                        return False
                      Right _ -> do
                        return True
                  Connecting -> do
                    logDebug $ "Connection already in progress for " <> relayURIToText relayURI
                    return False
              Nothing -> do
                logWarning $ "No connection state found for relay: " <> relayURIToText relayURI
                return False
          Nothing -> do
            reqChan <- newTChanIO
            resQueue <- newTQueueIO
            let newRelayData = RelayData (info relay) reqChan resQueue [] []
            modify @RelayPoolState $ \st' ->
              st' { relays = Map.insert relayURI newRelayData (relays st') }
            modify @WebSocketState $ \wsState ->
              wsState { connections = Map.insert relayURI (RelayConnectionState Connecting 0) (connections wsState) }
            result <- runClient relay reqChan resQueue
            case result of
              Left e -> do
                logError $ "Failed to connect to " <> relayURIToText relayURI <> ": " <> T.pack (show e)
                return False
              Right _ -> return True

      Nostr.Effects.RelayPool.Disconnect relayURI -> do
        st <- get @RelayPoolState
        case Map.lookup relayURI (relays st) of
          Just relayData -> do
            atomically $ writeTChan (requestChannel relayData) Nostr.Types.Disconnect
            logDebug $ T.pack $ "Disconnecting from " ++ T.unpack (relayURIToText relayURI) ++ " ..."

          Nothing -> return ()

      Nostr.Effects.RelayPool.DisconnectAll -> do
        logDebug $ "Disconnecting from all relays ..."
        st <- get @RelayPoolState
        forM_ (Map.elems $ relays st) $ \relayData ->
          atomically $ writeTChan (requestChannel relayData) Nostr.Types.Disconnect

      Nostr.Effects.RelayPool.SendEvent event rs -> do
        st <- get @RelayPoolState
        forM_ rs $ \relayURI -> do
          case Map.lookup relayURI (relays st) of
            Just relayData -> do
              atomically $ writeTChan (requestChannel relayData) (Nostr.Types.SendEvent event)
              logDebug $ "Sent event to channel: " <> T.pack (show event) <> " to relay: " <> relayURIToText relayURI
            Nothing -> logWarning $ "No channel found for relay: " <> relayURIToText relayURI

      GetRelays -> do
        st <- get @RelayPoolState
        wst <- get @WebSocketState
        relayInfo' <- forM (Map.toList $ relays st) $ \(relayURI, relayData) -> do
          let connStatus = maybe Disconnected connectionStatus (Map.lookup relayURI (connections wst))
          return (Relay relayURI (relayInfo relayData), connStatus)
        return relayInfo'

      StartSubscription relayURI filters' -> do
        st <- get @RelayPoolState
        case Map.lookup relayURI (relays st) of
          Just relayData -> do
            wst <- get @WebSocketState
            case Map.lookup relayURI (connections wst) of
              Just RelayConnectionState{connectionStatus = Connected} -> do
                subId' <- generateID 8
                atomically $ writeTChan (requestChannel relayData) (Subscribe $ Nostr.Types.Subscription filters' subId')
                logDebug $ "Starting new subscription: " <> subId' <> " on relay: " <> relayURIToText relayURI
                modify @RelayPoolState $ \st' ->
                  st' { relays = Map.adjust (\rd -> rd { subscriptions = subId' : subscriptions rd }) relayURI (relays st') }
                return $ Just (subId', responseQueue relayData)
              _ -> do
                logWarning $ "Cannot start subscription: Relay " <> relayURIToText relayURI <> " is not connected."
                return Nothing

          _ -> return Nothing

      StopSubscription subId' -> do
        st <- get @RelayPoolState
        let maybeRelayURI = Map.foldlWithKey' (\acc k v ->
                              if subId' `elem` subscriptions v then Just k else acc)
                            Nothing (relays st)
        case maybeRelayURI of
          Just relayURI -> do
            case Map.lookup relayURI (relays st) of
              Just relayData -> do
                atomically $ writeTChan (requestChannel relayData) (Close subId')
                logDebug $ "Closed subscription: " <> subId' <> " on relay: " <> relayURIToText relayURI
                modify @RelayPoolState $ \st' ->
                  st' { relays = Map.adjust (\rd -> rd { subscriptions = filter (/= subId') (subscriptions rd) }) relayURI (relays st') }
              Nothing -> logWarning $ "No channel found for relay: " <> relayURIToText relayURI
          Nothing -> return ()

      UnsubscribeAllSubscriptionsFromRelay relayURI -> do
        st <- get @RelayPoolState
        case Map.lookup relayURI (relays st) of
          Just relayData -> do
            forM_ (subscriptions relayData) $ \subId' -> do
              atomically $ writeTChan (requestChannel relayData) (Close subId')
              logDebug $ "Closed subscription: " <> subId' <> " on relay: " <> relayURIToText relayURI
            modify @RelayPoolState $ \st' ->
              st' { relays = Map.adjust (\rd -> rd { subscriptions = [] }) relayURI (relays st') }
          Nothing -> logError $ "No channel found for relay: " <> relayURIToText relayURI
