{-# LANGUAGE BlockArguments #-}

module Nostr.RelayPool where

import Control.Monad (forM, forM_, when)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.STM (atomically, flushTQueue, readTQueue, newTVarIO, readTVar, writeTVar)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get)
import Effectful.TH

import EffectfulQML
import Logging
import Nostr
import Nostr.GiftWrap
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Publisher
import Nostr.RelayConnection
import Nostr.Subscription
import Nostr.Types ( Event(..), Filter, Kind(..), Relay(..), RelayURI
                   , getUri, followListFilter, giftWrapFilter, metadataFilter, preferredDMRelaysFilter )
import Nostr.Util
import Types ( AppState(..), ConnectionState(..), Follow(..), RelayData(..)
             , RelayPoolState(..), SubscriptionEvent(..), emptyUpdates )
import RelayMgmt (RelayMgmt)
import RelayMgmt qualified as RM


data DisconnectReason = UserInitiated | ConnectionFailure
    deriving (Show, Eq)


-- | Effect for handling RelayPool operations.
data RelayPool :: Effect where
    -- General Relay Management
    ImportGeneralRelays :: PubKeyXO -> [Relay] -> Int -> RelayPool m ()
    AddGeneralRelay :: PubKeyXO -> RelayURI -> Bool -> Bool -> RelayPool m Bool
    RemoveGeneralRelay :: PubKeyXO -> RelayURI -> RelayPool m ()
    GetGeneralRelays :: PubKeyXO -> RelayPool m ([(Relay, ConnectionState)], Int)
    -- DM Relay Management
    AddDMRelay :: PubKeyXO -> RelayURI -> RelayPool m Bool
    RemoveDMRelay :: PubKeyXO -> RelayURI -> RelayPool m ()
    GetDMRelays :: PubKeyXO -> RelayPool m ([(Relay, ConnectionState)], Int)
    -- Connection Management
    Connect :: RelayURI -> RelayPool m ()
    Disconnect :: RelayURI -> RelayPool m ()
    DisconnectAll :: RelayPool m ()
    AwaitAtLeastOneConnected :: RelayPool m Bool
    -- Event Operations
    SendEvent :: Event -> RelayPool m ()

type instance DispatchOf RelayPool = Dynamic

makeEffect ''RelayPool


-- | RelayPoolEff
type RelayPoolEff es =
  ( State AppState :> es
  , State RelayPoolState :> es
  , Nostr :> es
  , RelayConnection :> es
  , Publisher :> es
  , RelayMgmt :> es
  , Subscription :> es
  , GiftWrap :> es
  , EffectfulQML :> es
  , Concurrent :> es
  , Logging :> es
  , Util :> es
  )


-- | Handler for relay pool effects.
runRelayPool
  :: RelayPoolEff es
  => Eff (RelayPool : es) a
  -> Eff es a
runRelayPool = interpret $ \_ -> \case
    ImportGeneralRelays pk rs ts -> RM.importGeneralRelays pk rs ts
    
    AddGeneralRelay pk relay' r w -> RM.addGeneralRelay pk relay' r w
    
    RemoveGeneralRelay pk r -> RM.removeGeneralRelay pk r

    GetGeneralRelays pk -> RM.getGeneralRelays pk

    AddDMRelay pk r -> RM.addDMRelay pk r

    RemoveDMRelay pk r -> RM.removeDMRelay pk r

    GetDMRelays pk -> RM.getDMRelays pk

    Connect r -> do
        res <- connectRelay r
        when res $ handleRelaySubscription r

    Nostr.RelayPool.Disconnect r -> disconnectRelay r

    DisconnectAll -> do
        st <- get @RelayPoolState
        forM_ (Map.toList $ activeConnections st) $ \(r, _) -> disconnectRelay r

    AwaitAtLeastOneConnected -> do
        let loop = do
                st <- get @RelayPoolState
                let states = map (connectionState . snd) $ Map.toList $ activeConnections st
                if any (== Connected) states
                    then return True
                    else if null states
                        then return False
                        else if all (== Disconnected) states
                            then return False
                            else do
                                threadDelay 100000  -- 100ms delay
                                loop
        loop

    Nostr.RelayPool.SendEvent event -> do
        kp <- getKeyPair
        let pk = keyPairToPubKeyXO kp

        case kind event of
            -- Events that should be broadcast to all relays
            PreferredDMRelays -> broadcast event
            RelayListMetadata -> broadcast event
            Metadata -> broadcast event
            -- Gift wrap events need special handling for DM relays
            GiftWrap -> publishGiftWrap event pk
            -- Default case: publish to outbox-capable relays (FollowList, ShortTextNote, etc.)
            _ -> publishToOutbox event


-- | Determine relay type and start appropriate subscriptions
handleRelaySubscription :: RelayPoolEff es => RelayURI -> Eff es ()
handleRelaySubscription r = do
    kp <- getKeyPair
    let pk = keyPairToPubKeyXO kp
    st <- get @AppState
    let followPks = maybe [] (map (\(Follow pk' _ _) -> pk')) $ Map.lookup pk (follows st)
    st' <- get @RelayPoolState
    
    -- Check if it's a DM relay
    let isDM = any (\(_, (relays, _)) -> 
            any (\relay -> getUri relay == r) relays)
            (Map.toList $ dmRelays st')
    
    -- Check if it's an inbox-capable relay
    let isInbox = any (\(_, (relays, _)) -> 
            any (\relay -> case relay of
                InboxRelay uri -> uri == r
                InboxOutboxRelay uri -> uri == r
                _ -> False) relays)
            (Map.toList $ generalRelays st')
    
    -- Start appropriate subscriptions based on relay type
    let fs = if isDM then Just $ createDMRelayFilters pk followPks
            else if isInbox then Just $ createInboxRelayFilters pk followPks
            else Nothing

    logInfo $ "Starting subscription for " <> r <> " with filters " <> T.pack (show fs)

    case fs of
        Just fs' -> do
            subId' <- newSubscriptionId
            mq <- subscribe r subId' (createDMRelayFilters pk followPks)
            case mq of
                Just q -> do
                    shouldStop <- newTVarIO False

                    let loop = do
                            e <- atomically $ readTQueue q
                            es <- atomically $ flushTQueue q

                            updates <- fmap mconcat $ forM (e : es) $ \case
                                EventAppeared event' -> handleEvent r subId' fs' event'
                                SubscriptionEose -> return emptyUpdates
                                SubscriptionClosed _ -> do
                                    atomically $ writeTVar shouldStop True
                                    return emptyUpdates
                            
                            shouldStopNow <- atomically $ readTVar shouldStop

                            if shouldStopNow
                                then return ()
                                else loop

                            notify updates
                    loop
                Nothing -> logWarning $ "Failed to start subscription for " <> r
        Nothing -> return () -- Outbox only relay or unknown relay, no subscriptions needed



-- | Create DM relay subscription filters
createDMRelayFilters :: PubKeyXO -> [PubKeyXO] -> [Filter]
createDMRelayFilters xo followedPubKeys =
    [ metadataFilter (xo : followedPubKeys)
    , preferredDMRelaysFilter (xo : followedPubKeys)
    , giftWrapFilter xo
    ]

-- | Create inbox relay subscription filters
createInboxRelayFilters :: PubKeyXO -> [PubKeyXO] -> [Filter]
createInboxRelayFilters xo followedPubKeys =
    [ followListFilter (xo : followedPubKeys)
    , metadataFilter (xo : followedPubKeys)
    , preferredDMRelaysFilter (xo : followedPubKeys)
    ]
