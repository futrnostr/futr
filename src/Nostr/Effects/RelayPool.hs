{-# LANGUAGE BlockArguments #-}

module Nostr.Effects.RelayPool where

import Control.Monad (forM_, unless)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TQueue, atomically, newTChanIO, newTQueueIO, writeTChan)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, modify)
import Effectful.TH

import AppState (RelayPoolState(..), RelayData(..), initialRelayPoolState)
import Nostr.Effects.IDGen
import Nostr.Effects.Logging
import Nostr.Effects.WebSocket
import Nostr.Types

-- | Effect for handling RelayPool operations.
data RelayPool :: Effect where
    AddRelay :: Relay -> RelayPool m ()
    Connect :: Relay -> RelayPool m ()
    Disconnect :: Relay -> RelayPool m ()
    DisconnectAll :: RelayPool m ()
    SendEvent :: Event -> [RelayURI] -> RelayPool m ()
    GetRelays :: RelayPool m [(Relay, Bool)]
    StartSubscription :: RelayURI -> [Filter] -> RelayPool m (Maybe (SubscriptionId, TQueue Response))
    StopSubscription :: SubscriptionId -> RelayPool m ()
    UnsubscribeAllSubscriptionsFromRelay :: RelayURI -> RelayPool m ()

type instance DispatchOf RelayPool = Dynamic

makeEffect ''RelayPool

type RelayPoolEff es = (WebSocket :> es, Concurrent :> es, Logging :> es, IDGen :> es)

data RelayPoolError = RelayNotFound RelayURI
  deriving (Show, Eq)

-- | Handler for relay pool effects.
runRelayPool
  :: RelayPoolEff es
  => Eff (RelayPool : State RelayPoolState : es) a
  -> Eff es a
runRelayPool action = evalState initialRelayPoolState $ interpret handleRelayPool action
  where
    handleRelayPool :: forall es. RelayPoolEff es => EffectHandler RelayPool (State RelayPoolState : es)
    handleRelayPool _ = \case
      AddRelay relay -> do
        st <- get @RelayPoolState
        let relayURI = uri relay
            existingRelays = relays st
        unless (Map.member relayURI existingRelays) do
          reqChan <- newTChanIO
          resQueue <- newTQueueIO
          let newRelayData = RelayData False (info relay) reqChan resQueue [] []
              updatedRelays = Map.insert relayURI newRelayData existingRelays
          modify @RelayPoolState $ \st' -> st' { relays = updatedRelays }

      Connect relay -> do
        let relayURI = uri relay
        st <- get @RelayPoolState
        case Map.lookup relayURI (relays st) of
          Just relayData -> do
            modify @RelayPoolState $ \st' ->
              st' { relays = Map.adjust (\rd -> rd { connected = True }) relayURI (relays st') }
            runClient relay (requestChannel relayData) (responseQueue relayData)
          Nothing -> do
            reqChan <- newTChanIO
            resQueue <- newTQueueIO
            let newRelayData = RelayData False (info relay) reqChan resQueue [] []
            modify @RelayPoolState $ \st' ->
              st' { relays = Map.insert relayURI newRelayData (relays st') }
            runClient relay reqChan resQueue

      Nostr.Effects.RelayPool.Disconnect relay -> do
        let relayURI = uri relay
        logDebug $ T.pack $ "Disconnecting from " ++ T.unpack (relayName relay) ++ " ..."
        modify @RelayPoolState $ \st ->
          st { relays = Map.adjust (\rd -> rd { connected = False }) relayURI (relays st) }
        st <- get @RelayPoolState
        case Map.lookup relayURI (relays st) of
          Just relayData -> atomically $ writeTChan (requestChannel relayData) Nostr.Types.Disconnect
          Nothing -> return ()

      Nostr.Effects.RelayPool.DisconnectAll -> do
        logDebug $ "Disconnecting from all relays ..."
        modify @RelayPoolState $ \st ->
          st { relays = Map.map (\rd -> rd { connected = False }) (relays st) }
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
        return $ map (\(uri', rd) -> (Relay uri' (relayInfo rd), connected rd)) $ Map.toList (relays st)

      StartSubscription relayURI filters' -> do
        st <- get @RelayPoolState
        subId' <- generateID 8
        case Map.lookup relayURI (relays st) of
          Just relayData -> do
            atomically $ writeTChan (requestChannel relayData) (Subscribe $ Nostr.Types.Subscription filters' subId')
            logDebug $ "Starting new subscription: " <> subId' <> " on relay: " <> relayURIToText relayURI
            modify @RelayPoolState $ \st' ->
              st' { relays = Map.adjust (\rd -> rd { subscriptions = subId' : subscriptions rd }) relayURI (relays st') }
            return $ Just (subId', responseQueue relayData)
          Nothing -> do
            logError $ "No channel found for relay: " <> relayURIToText relayURI
            return Nothing

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
                logDebug $ "Closed subscription: " <> subId' <> " on relay: " <> T.pack (show relayURI)
                modify @RelayPoolState $ \st' ->
                  st' { relays = Map.adjust (\rd -> rd { subscriptions = filter (/= subId') (subscriptions rd) }) relayURI (relays st') }
              Nothing -> logWarning $ "No channel found for relay: " <> T.pack (show relayURI)
          Nothing -> return ()

      UnsubscribeAllSubscriptionsFromRelay relayURI -> do
        st <- get @RelayPoolState
        case Map.lookup relayURI (relays st) of
          Just relayData -> do
            forM_ (subscriptions relayData) $ \subId' -> do
              atomically $ writeTChan (requestChannel relayData) (Close subId')
              logDebug $ "Closed subscription: " <> subId' <> " on relay: " <> T.pack (show relayURI)
            modify @RelayPoolState $ \st' ->
              st' { relays = Map.adjust (\rd -> rd { subscriptions = [] }) relayURI (relays st') }
          Nothing -> logError $ "No channel found for relay: " <> T.pack (show relayURI)
