{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Effects.RelayPool where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TChan, TQueue, atomically, dupTChan, newTChanIO, newTQueueIO, writeTChan)
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
    SendEvent :: Event -> RelayPool m ()
    Subscribe :: [Filter] -> RelayPool m SubscriptionId
    Unsubscribe :: SubscriptionId -> RelayPool m ()
    ListRelays :: RelayPool m [Relay]

type instance DispatchOf RelayPool = Dynamic

makeEffect ''RelayPool

type SubscribtionId = Text

-- | State for RelayPool handling.
data RelayPoolState = RelayPoolState
    { relays        :: [Relay]
    , subscriptions :: [SubscribtionId]
    , incomingQueue :: TQueue Response
    , broadcastChan :: TChan Request
    }

-- | Handler for relay pool effects.
runRelayPool
  :: forall (es :: [Effect]) a.
     (Concurrent :> es, IDGen :> es, IOE :> es, Logging :> es, WebSocket :> es)
  => Eff (RelayPool : State RelayPoolState : es) a
  -> Eff es a
runRelayPool action = do
  queue <- newTQueueIO
  bcastChan <- newTChanIO
  let initialState = RelayPoolState
        { relays = []
        , subscriptions = []
        , incomingQueue = queue
        , broadcastChan = bcastChan
        }
  evalState initialState $ interpret handleRelayPool action
  where
    handleRelayPool
      :: (Concurrent :> es, IDGen :> es, IOE :> es, Logging :> es, WebSocket :> es)
      => EffectHandler RelayPool (State RelayPoolState : es)
    handleRelayPool _ = \case
      AddRelay relay -> do
        modify $ \st ->
          let existingRelays = relays st
              updatedRelays = if relay `elem` existingRelays
                              then existingRelays
                              else relay { connected = False } : existingRelays
          in st { relays = updatedRelays }

      Connect relay -> do
        logDebug $ T.pack $ "trying to connect to " ++ (T.unpack $ relayName relay) ++ " ..."
        modify $ \st -> st { relays = map (\r -> if r `sameRelay` relay then relay { connected = True } else r) (relays st) }
        st <- get
        chan <- atomically $ dupTChan (broadcastChan st)
        runClient relay chan (incomingQueue st)

      Nostr.Effects.RelayPool.Disconnect relay -> do
        logDebug $ T.pack $ "Disconnecting from " ++ T.unpack (relayName relay) ++ " ..."
        modify $ \st -> st { relays = map (\r -> if uri r == uri relay then r { connected = False } else r) (relays st) }
        st <- get
        atomically $ writeTChan (broadcastChan st) (Nostr.Types.Disconnect relay)

      Nostr.Effects.RelayPool.DisconnectAll -> do
        logDebug $ T.pack $ "Disconnecting from all relays ..."
        st <- get
        let rs = map (\r -> r { connected = True }) (relays st)
        modify $ \st' -> st' { relays = map (\r -> r { connected = False }) (relays st') }
        forM_ rs $ \r -> atomically $ writeTChan (broadcastChan st) (Nostr.Types.Disconnect r)

      Nostr.Effects.RelayPool.SendEvent event -> do
        st <- get
        atomically $ writeTChan (broadcastChan st) (Nostr.Types.SendEvent event)
        logDebug $ T.pack $ "Sent event: " ++ show event

      Nostr.Effects.RelayPool.Subscribe filters' -> do
        logDebug $ "Starting new subscription"
        subId' <- generateID 8
        modify $ \st -> st { subscriptions = subId' : subscriptions st }
        st <- get
        atomically $ writeTChan (broadcastChan st) (Nostr.Types.Subscribe (Subscription filters' subId'))
        return subId'

      Unsubscribe subId' -> do
        logDebug $ "Stopping subscription"
        modify $ \st -> st { subscriptions = filter (\s -> s /= subId') (subscriptions st) }
        st <- get
        atomically $ writeTChan (broadcastChan st) (Nostr.Types.Close subId')

      ListRelays -> do
        st <- get
        return $ relays st
