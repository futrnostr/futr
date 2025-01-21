{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nostr.InboxModel where

import Control.Monad (forM, forM_, unless, void, when)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (pack,unpack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, put, modify)
import Effectful.TH

import Logging
import Nostr.Event
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.RelayConnection (RelayConnection, connectRelay, disconnectRelay)
import Nostr.Subscription (Subscription, handleEvent, newSubscriptionId, stopSubscription, stopAllSubscriptions, subscribe, userPostsFilter, giftWrapFilter, mentionsFilter, profilesFilter)
import Nostr.Types
  ( RelayURI
  , Relay(..)
  , Event(..)
  , Filter(..)
  , Kind(..)
  , emptyFilter
  , getUri
  , isInboxCapable
  , isOutboxCapable
  )
import Nostr.Util
import QtQuick (QtQuick,UIUpdates, emptyUpdates, notify)
import Store.Lmdb (LmdbStore, getFollows, getGeneralRelays, getDMRelays, getLatestTimestamp)
import Types (AppState(..), ConnectionState(..), Follow(..), SubscriptionDetails(..), RelayPool(..), RelayData(..), SubscriptionEvent(..), initialRelayPool)


-- | InboxModel effects
data InboxModel :: Effect where
  StartInboxModel :: InboxModel m ()
  StopInboxModel :: InboxModel m ()
  AwaitAtLeastOneConnected :: InboxModel m Bool

type instance DispatchOf InboxModel = Dynamic

makeEffect ''InboxModel

type InboxModelEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
  , Subscription :> es
  , RelayConnection :> es
  , Logging :> es
  , Concurrent :> es
  , Util :> es
  , QtQuick :> es
  )

-- | Run InboxModel
runInboxModel :: InboxModelEff es => Eff (InboxModel : es) a -> Eff es a
runInboxModel = interpret $ \_ -> \case
  StartInboxModel -> do
    kp <- getKeyPair
    let xo = keyPairToPubKeyXO kp
    void $ async $ do
      initializeSubscriptions xo
      eventLoop xo
    
  StopInboxModel -> do
    st <- get @RelayPool
    forM_ (Map.keys $ activeConnections st) $ \relayUri -> do
      disconnectRelay relayUri
    put @RelayPool initialRelayPool

  AwaitAtLeastOneConnected -> do
    let loop = do
          st <- get @RelayPool
          let states = map (connectionState . snd) $ Map.toList $ activeConnections st
          if any (== Connected) states
              then return True
              else if null states
                  then return False
                  else if all (== Disconnected) states
                      then return False
                      else do
                          threadDelay 50000  -- 50ms delay
                          loop
    loop

-- | Initialize subscriptions
initializeSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
initializeSubscriptions xo = do
  follows <- getFollows xo
  let followList = map pubkey follows

  inboxRelays <- getGeneralRelays xo
  let ownInboxRelayURIs = [ getUri relay | relay <- inboxRelays, isInboxCapable relay ]

  logDebug $ "Initializing subscriptions for Inbox Relays: " <> pack (show ownInboxRelayURIs)

  dmRelays <- getDMRelays xo
  logDebug $ "Initializing subscriptions for DM Relays: " <> pack (show (map getUri dmRelays))

  forM_ dmRelays $ \relay -> do
    let relayUri = getUri relay
    connected <- connectRelay relayUri
    if connected
        then do
            logInfo $ "Connected to DM Relay: " <> relayUri
            subscribeToGiftwraps relayUri xo
            logInfo $ "Subscribed to Giftwraps on relay: " <> relayUri
        else do
            logError $ "Failed to connect to DM Relay: " <> relayUri

  forM_ inboxRelays $ \relay -> do
    let relayUri = getUri relay
    connected <- connectRelay relayUri
    if connected
        then do
            logInfo $ "Connected to Inbox Relay: " <> relayUri
            when (isInboxCapable relay) $ do
              subscribeToMentions relayUri xo
              logInfo $ "Subscribed to Mentions on relay: " <> relayUri
        else do
            logError $ "Failed to connect to Inbox Relay: " <> relayUri

  followRelayMap <- buildRelayPubkeyMap followList ownInboxRelayURIs
  logDebug $ "Building Relay-PubKey Map: " <> pack (show followRelayMap)

  forM_ (Map.toList followRelayMap) $ \(relayUri, pubkeys) -> do
    connected <- connectRelay relayUri
    if connected
        then do
            logInfo $ "Connected to Follow Relay: " <> relayUri
            subscribeToRelay relayUri pubkeys
            logInfo $ "Subscribed to Relay: " <> relayUri <> " for PubKeys: " <> pack (show pubkeys)
        else do
            logError $ "Failed to connect to Follow Relay: " <> relayUri

-- | Maximum number of outbox relays to consider per contact
maxRelaysPerContact :: Int
maxRelaysPerContact = 3

-- | Build a map from relay URI to pubkeys, prioritizing existing inbox relays
buildRelayPubkeyMap :: InboxModelEff es => [PubKeyXO] -> [RelayURI] -> Eff es (Map.Map RelayURI [PubKeyXO])
buildRelayPubkeyMap pubkeys ownInboxRelays = do
  relayPubkeyPairs <- forM pubkeys $ \pk -> do
    relays <- getGeneralRelays pk
    let outboxRelayURIs = [ getUri relay | relay <- relays, isOutboxCapable relay ]
    -- Prioritize relays that are in our inbox relays list
    let (prioritized, other) = partition (`elem` ownInboxRelays) outboxRelayURIs
    let selectedRelays = take maxRelaysPerContact $ prioritized ++ other
    return (pk, selectedRelays)
  
  -- Build the map and filter out empty lists
  return $ Map.filter (not . null) $ foldr (\(pk, relays) acc ->
    foldr (\relay -> Map.insertWith (<>) relay [pk]) acc relays
    ) Map.empty relayPubkeyPairs


-- | Subscribe to mentions on a relay
subscribeToMentions :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToMentions relayUri xo = do
  subId <- newSubscriptionId
  mQueue <- subscribe relayUri subId $ mentionsFilter xo
  case mQueue of
    Just _ -> logDebug $ "Subscribed to mentions on " <> relayUri
    Nothing -> logWarning $ "Failed to subscribe to mentions on " <> relayUri


-- | Subscribe to profiles and posts for a relay
subscribeToRelay :: InboxModelEff es => RelayURI -> [PubKeyXO] -> Eff es ()
subscribeToRelay relayUri pubkeys = do
  -- Subscribe to profiles
  profileSubId <- newSubscriptionId
  profileLastTimestamp <- getSubscriptionTimestamp pubkeys [RelayListMetadata, PreferredDMRelays, FollowList]
  mProfileQueue <- subscribe relayUri profileSubId $ profilesFilter pubkeys profileLastTimestamp
  case mProfileQueue of
    Just _ -> logDebug $ "Subscribed to profiles on " <> relayUri
    Nothing -> logWarning $ "Failed to subscribe to profiles on " <> relayUri

  -- Subscribe to posts
  postsSubId <- newSubscriptionId
  postsLastTimestamp <- getSubscriptionTimestamp pubkeys [ShortTextNote, Repost, EventDeletion]
  mPostsQueue <- subscribe relayUri postsSubId $ userPostsFilter pubkeys postsLastTimestamp
  case mPostsQueue of
    Just _ -> logDebug $ "Subscribed to posts on " <> relayUri
    Nothing -> logWarning $ "Failed to subscribe to posts on " <> relayUri


-- | Subscribe to giftwrap events on a relay
subscribeToGiftwraps :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToGiftwraps relayUri xo = do
  subId <- newSubscriptionId
  lastTimestamp <- getSubscriptionTimestamp [xo] [GiftWrap]
  mGiftwrapQueue <- subscribe relayUri subId $ giftWrapFilter xo lastTimestamp
  case mGiftwrapQueue of
    Just _ -> logDebug $ "Subscribed to giftwraps on " <> relayUri
    Nothing -> logWarning $ "Failed to subscribe to giftwraps on " <> relayUri


-- | Function to compute the minimum latest timestamp across multiple PubKeys and Kinds
getSubscriptionTimestamp :: InboxModelEff es => [PubKeyXO] -> [Kind] -> Eff es (Maybe Int)
getSubscriptionTimestamp pubKeys kinds = do
    timestampsPerPubKey <- forM pubKeys $ \pk -> do
        mTs <- getLatestTimestamp pk kinds
        return mTs
    let validTimestamps = catMaybes timestampsPerPubKey
    return $ if null validTimestamps
             then Nothing
             else Just (minimum validTimestamps)


-- | Event loop to handle incoming events and updates
eventLoop :: InboxModelEff es => PubKeyXO -> Eff es ()
eventLoop xo = do
  shouldStopVar <- newTVarIO False
  let loop = do
        -- Collect events from all subscriptions
        events <- collectEvents
        -- Process each event
        forM_ events $ \(relayUri, event) -> do
          case event of
            EventAppeared event' -> do
              -- Handle the event
              updates <- handleEvent relayUri event'
              -- If it's a FollowList or PreferredDMRelays event from ourselves, update subscriptions
              when ((kind event' == FollowList || kind event' == PreferredDMRelays) && pubKey event' == xo) $ do
                logInfo "Detected updated FollowList or PreferredDMRelays; updating subscriptions."
                updateSubscriptions xo
              -- Notify the UI
              notify updates
            SubscriptionEose -> return ()
            SubscriptionClosed _ -> do
              atomically $ writeTVar shouldStopVar True
        -- Check if we should stop
        shouldStop <- atomically $ readTVar shouldStopVar
        when (not shouldStop) loop
  loop

-- | Collect events from all active subscriptions
collectEvents :: InboxModelEff es => Eff es [(RelayURI, SubscriptionEvent)]
collectEvents = do
  st <- get @RelayPool
  let relaySubs = [ (relayUri, sd)
                  | (relayUri, rd) <- Map.toList (activeConnections st)
                  , sd <- Map.elems (activeSubscriptions rd)
                  ]
  events <- forM relaySubs $ \(relayUri, sd) -> do
    e <- atomically $ tryReadTQueue (responseQueue sd)
    es <- atomically $ flushTQueue (responseQueue sd)
    let allEvents = maybeToList e ++ es
    return [ (relayUri, ev) | ev <- allEvents ]
  return $ concat events

-- | Update subscriptions when follows or preferred DM relays change
updateSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateSubscriptions xo = do
  -- Update general subscriptions
  updateGeneralSubscriptions xo

  -- Update DM subscriptions
  updateDMSubscriptions xo

-- | Update general subscriptions based on current follow list
updateGeneralSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateGeneralSubscriptions xo = do
  follows <- getFollows xo
  let followList = map pubkey follows

  inboxRelays <- getGeneralRelays xo
  let ownInboxRelayURIs = [ getUri relay | relay <- inboxRelays, isInboxCapable relay ]

  forM_ inboxRelays $ \relay -> do
    when (isInboxCapable relay) $ do
      let relayUri = getUri relay
      connected <- connectRelay relayUri
      when connected $ subscribeToMentions relayUri xo

  -- Build optimized relay map for follows, prioritizing our inbox relays
  newRelayPubkeyMap <- buildRelayPubkeyMap followList ownInboxRelayURIs

  st <- get @RelayPool
  let currentRelays = Map.keysSet (activeConnections st)
  let newRelays = Map.keysSet newRelayPubkeyMap

  let relaysToAdd = Set.difference newRelays currentRelays
  let relaysToRemove = Set.difference currentRelays newRelays
  let relaysToUpdate = Set.intersection currentRelays newRelays

  forM_ (Set.toList relaysToRemove) $ \relayUri -> do
    disconnectRelay relayUri

  forM_ (Set.toList relaysToAdd) $ \relayUri -> do
    connected <- connectRelay relayUri
    when connected $ do
      let pubkeys = Map.findWithDefault [] relayUri newRelayPubkeyMap
      subscribeToRelay relayUri pubkeys

  forM_ (Set.toList relaysToUpdate) $ \relayUri -> do
    let newPubkeys = Set.fromList $ Map.findWithDefault [] relayUri newRelayPubkeyMap
    rd <- getRelayData relayUri
    let currentPubkeys = Set.fromList $ getSubscribedPubkeys rd
    when (newPubkeys /= currentPubkeys) $ do
      -- Unsubscribe existing subscriptions
      stopAllSubscriptions relayUri
      -- Subscribe with new pubkeys
      subscribeToRelay relayUri (Set.toList newPubkeys)

-- | Update DM subscriptions
updateDMSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateDMSubscriptions xo = do
    kp <- getKeyPair
    let ownPubkey = keyPairToPubKeyXO kp

    when (xo == ownPubkey) $ do
        dmRelays <- getDMRelays xo
        let dmRelayURIs = map getUri dmRelays
        let dmRelaySet = Set.fromList dmRelayURIs

        currentConnections <- gets @RelayPool activeConnections

        let relaysWithGiftwrap = 
              [ (relayUri, rd)
              | (relayUri, rd) <- Map.toList currentConnections
              , any (\sd -> GiftWrap `elem` fromMaybe [] (kinds $ subscriptionFilter sd)) 
                    (Map.elems $ activeSubscriptions rd)
              ]

        forM_ relaysWithGiftwrap $ \(relayUri, rd) -> 
            unless (relayUri `Set.member` dmRelaySet) $ do
                let giftwrapSubs = 
                      [ subId 
                      | (subId, sd) <- Map.toList (activeSubscriptions rd)
                      , GiftWrap `elem` fromMaybe [] (kinds $ subscriptionFilter sd)
                      ]

                forM_ giftwrapSubs stopSubscription

                let remainingSubCount = Map.size (activeSubscriptions rd) - length giftwrapSubs
                when (remainingSubCount == 0) $ do
                    disconnectRelay relayUri

        let currentRelaySet = Map.keysSet currentConnections
        let relaysToAdd = Set.difference dmRelaySet currentRelaySet

        forM_ (Set.toList relaysToAdd) $ \relayUri -> do
            connected <- connectRelay relayUri
            when connected $ do
                subscribeToGiftwraps relayUri xo


-- | Get RelayData for a relay
getRelayData :: InboxModelEff es => RelayURI -> Eff es RelayData
getRelayData relayUri = do
  st <- get @RelayPool
  return $ fromMaybe (error $ "No RelayData for " <> unpack relayUri) $ Map.lookup relayUri (activeConnections st)


-- | Get pubkeys from subscriptions in RelayData
getSubscribedPubkeys :: RelayData -> [PubKeyXO]
getSubscribedPubkeys rd =
  concatMap (fromMaybe [] . authors . subscriptionFilter) (Map.elems $ activeSubscriptions rd)
