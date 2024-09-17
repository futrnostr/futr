{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Effects.RelayPool where

import Control.Monad (forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TChan, TQueue, atomically, newTChanIO, newTQueueIO, writeTChan)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, evalState, get, modify)
import Effectful.TH

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
    Subscribe :: [Filter] -> [Relay] -> RelayPool m SubscriptionId
    Unsubscribe :: SubscriptionId -> RelayPool m ()
    ListRelays :: RelayPool m [(Relay, Bool)]

type instance DispatchOf RelayPool = Dynamic

makeEffect ''RelayPool

type SubscribtionId = Text

-- | State for RelayPool handling.
data RelayPoolState = RelayPoolState
    { relays        :: [(Relay, Bool)]
    , subscriptions :: Map SubscribtionId (TQueue Response, [Relay])
    , incomingQueue :: TQueue Response
    , relayChannels :: Map RelayURI (TChan Request)
    }

-- | Handler for relay pool effects.
runRelayPool
  :: forall (es :: [Effect]) a.
     (Concurrent :> es, IDGen :> es, IOE :> es, Logging :> es, WebSocket :> es)
  => Eff (RelayPool : State RelayPoolState : es) a
  -> Eff es a
runRelayPool action = do
  queue <- newTQueueIO
  let initialState = RelayPoolState
        { relays = []
        , subscriptions = Map.empty
        , incomingQueue = queue
        , relayChannels = Map.empty
        }
  evalState initialState $ interpret handleRelayPool action
  where
    handleRelayPool
      :: (Concurrent :> es, IDGen :> es, IOE :> es, Logging :> es, WebSocket :> es)
      => EffectHandler RelayPool (State RelayPoolState : es)
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
        logDebug $ T.pack $ "trying to connect to " ++ (T.unpack $ relayName relay) ++ " ..."
        modify $ \st -> st { relays = map (\(r, c) -> if r `sameRelay` relay then (r, True) else (r, c)) (relays st) }
        st <- get
        case Map.lookup (uri relay) (relayChannels st) of
          Just chan -> runClient relay chan (incomingQueue st)
          Nothing -> logError $ "Could not found channel to communicate to relay " <> relayName relay

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

      Nostr.Effects.RelayPool.Subscribe filters' rs -> do
        subId' <- generateID 8
        queue <- newTQueueIO
        modify $ \st -> st { subscriptions = Map.insert subId' (queue, rs) (subscriptions st) }
        st <- get
        forM_ rs $ \relay -> do
          case Map.lookup (uri relay) (relayChannels st) of
            Just chan -> do
              atomically $ writeTChan chan (Nostr.Types.Subscribe (Subscription filters' subId'))
              logDebug $ "Started new subscription: " <> subId' <> " on relay: " <> relayName relay
            Nothing -> return ()
        return subId'

      Unsubscribe subId' -> do
        st <- get
        case Map.lookup subId' (subscriptions st) of
          Just (_, rs) ->
            forM_ rs $ \relay -> case Map.lookup (uri relay) (relayChannels st) of
              Just chan -> do
                atomically $ writeTChan chan (Nostr.Types.Close subId')
                logDebug $ "Stopping subscription: " <> subId' <> " on relay: " <> relayName relay
              Nothing -> return ()
          Nothing -> logWarning $ "Unsubscription of non-existent subscription seen: " <> subId'

      ListRelays -> do
        st <- get
        return $ relays st
