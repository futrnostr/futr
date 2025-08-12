module Nostr.SubscriptionHandler where

import Control.Monad (forM_, unless, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (atomically, newTVarIO, readTVar, writeTChan, writeTVar, modifyTVar)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Prelude hiding (until)

import KeyMgmt (KeyMgmt)
import Logging
import Nostr.Event (Event(..), EventId(..))
import Nostr.EventHandler (EventHandler, handleEvent)
import Nostr.Relay (RelayURI)
import Nostr.RelayPool (RelayPool, RelayPoolState(..), SubscriptionState(..))
import Nostr.Subscription
import Nostr.Types (Filter(..), SubscriptionId)
import Nostr.Types qualified as NT
import Nostr.Util
import QtQuick
import Store.Lmdb
import Types

-- | Subscription effects
data SubscriptionHandler :: Effect where
    HandleSubscription :: SubscriptionId -> SubscriptionHandler m ()
    HandleSubscriptionUntilEOSE :: SubscriptionId -> SubscriptionHandler m ()

type instance DispatchOf SubscriptionHandler = Dynamic


handleSubscription :: SubscriptionHandler :> es => SubscriptionId -> Eff es ()
handleSubscription subId = send $ HandleSubscription subId

handleSubscriptionUntilEOSE :: SubscriptionHandler :> es => SubscriptionId -> Eff es ()
handleSubscriptionUntilEOSE subId = send $ HandleSubscriptionUntilEOSE subId


-- | SubscriptionEff
type SubscriptionHandlerEff es =
  ( Subscription :> es
  , EventHandler :> es
  , LmdbStore :> es
  , KeyMgmt :> es
  , State RelayPoolState :> es
  , RelayPool :> es
  , Util :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  )

-- | Handler for subscription effects.
runSubscriptionHandler
  :: SubscriptionHandlerEff es
  => Eff (SubscriptionHandler : es) a
  -> Eff es a
runSubscriptionHandler = interpret $ \_ -> \case
    HandleSubscription subId' -> do
        subs <- gets @RelayPoolState subscriptions
        case Map.lookup subId' subs of
            Nothing -> pure ()
            Just _ -> pure ()

    HandleSubscriptionUntilEOSE subId' -> do
        subs <- gets @RelayPoolState subscriptions
        case Map.lookup subId' subs of
            Nothing -> pure ()
            Just _ -> pure ()
