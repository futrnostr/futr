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
import Effectful.Concurrent.STM (TChan, TQueue, atomically, writeTChan, newTChanIO, newTQueueIO)
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
        , subscriptions = []
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
        newChan <- newTChanIO
        modify $ \st ->
          let existingRelays = relays st
              newRelay = relay { connected = False }
              updatedRelays = if relay `elem` existingRelays
                              then existingRelays
                              else newRelay : existingRelays
              updatedChannels = Map.insert (uri newRelay) newChan (relayChannels st)
          in st { relays = updatedRelays, relayChannels = updatedChannels }

      Connect relay -> do
        logDebug $ T.pack $ "trying to connect to " ++ (T.unpack $ relayName relay) ++ " ..."
        st <- get
        case Map.lookup (uri relay) (relayChannels st) of
          Just chan -> do
            modify $ \st' ->
              let updatedRelay = relay { connected = True }
                  updatedRelays = map (\r -> if r `sameRelay` relay then updatedRelay else r) (relays st)
              in st' { relays = updatedRelays }
            case (extractHostname relay, extractScheme relay) of
              (Just host', Just scheme') -> do
                let args = WebSocketConnArgs
                      { host = T.unpack host'
                      , port = extractPort relay
                      , path = T.unpack $ extractPath relay
                      , scheme = T.unpack scheme'
                      }
                runClient chan (incomingQueue st) args

              _ -> logError $ "Invalid relay configuration: " <> T.pack (show relay)
          Nothing -> logError $ T.pack $ "No channel found for relay: " ++ T.unpack (relayName relay)

      Nostr.Effects.RelayPool.Disconnect relay -> do
        logDebug $ T.pack $ "Disconnecting from " ++ T.unpack (relayName relay) ++ " ..."
        modify $ \st -> st { relays = map (\r -> if uri r == uri relay then r { connected = False } else r) (relays st) }
        st <- get
        case Map.lookup (uri relay) (relayChannels st) of
          Just chan -> atomically $ writeTChan chan Nostr.Types.Disconnect
          Nothing -> return ()

      Nostr.Effects.RelayPool.DisconnectAll -> do
        logDebug $ T.pack $ "Disconnecting from all relays ..."
        modify $ \st -> st { relays = map (\r -> r { connected = False }) (relays st) }
        st <- get
        let chans = relayChannels st
        forM_ (Map.elems chans) $ \chan -> atomically $ writeTChan chan Nostr.Types.Disconnect

      Nostr.Effects.RelayPool.SendEvent event -> do
        st <- get
        let chans = relayChannels st
        forM_ (Map.elems chans) $ \chan -> atomically $ writeTChan chan (Nostr.Types.SendEvent event)
        logDebug $ T.pack $ "Sent event: " ++ show event

      Nostr.Effects.RelayPool.Subscribe filters' -> do
        logDebug $ "Starting new subscription"
        subId' <- generateID 8
        modify $ \st -> st { subscriptions = subId' : subscriptions st }
        st <- get
        let chans = relayChannels st
        forM_ (Map.elems chans) $ \chan -> atomically $ writeTChan chan (Nostr.Types.Subscribe $ Subscription filters' subId')
        return subId'

      Unsubscribe subId' -> do
        logDebug $ "Stopping subscription"
        modify $ \st -> st { subscriptions = filter (\s -> s /= subId') (subscriptions st) }
        let request = Close subId'
        st <- get
        let chans = relayChannels st
        forM_ (Map.elems chans) $ \chan -> atomically $ writeTChan chan request

      ListRelays -> do
        st <- get
        return $ relays st
