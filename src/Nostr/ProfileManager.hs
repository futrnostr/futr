module Nostr.ProfileManager where

import Control.Monad (forM_, void, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.List (partition)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, Async)
import Effectful.Concurrent.STM ( TChan, TVar, atomically, newTChanIO, writeTChan
                                , newTVarIO, readTVar, writeTVar, tryReadTChan )
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets, modify, put)

import KeyMgmt (KeyMgmt)
import Logging (Logging)
import Nostr
import Nostr.Event (Kind(..))
import Nostr.EventProcessor (handleEvent)
import Nostr.Keys (PubKeyXO)
import Nostr.Profile (Profile)
import Nostr.Relay ( ConnectionState(..), RelayConnection, RelayData(..), RelayPool(..)
                   , connect, subscribeTemporary, waitForCompletion )
import Nostr.Types (RelayURI, metadataFilter)
import Nostr.Util
import QtQuick (QtQuick, QtQuickState)
import Store.Lmdb (LmdbStore)
import Store.Lmdb qualified as Lmdb
import Types

-- | Request for profile fetching
data ProfileFetchRequest = ProfileFetchRequest
    { requestPubKey :: PubKeyXO
    , requestRelayHints :: [RelayURI]
    }

-- | State for the profile manager with queue and batching
data ProfileManagerState = ProfileManagerState
    { lastFetchAttempts :: Map.Map PubKeyXO Int  -- pubkey -> timestamp of last fetch attempt
    , pendingFetches :: Set.Set PubKeyXO
    , fetchQueue :: TChan ProfileFetchRequest
    , activeFetches :: TVar Int  -- current number of active fetches (max 3)
    , batchedRequests :: TVar (Map.Map RelayURI [PubKeyXO])  -- relay -> list of pubkeys to fetch
    , batchTimer :: TVar (Maybe (Async ()))  -- timer for batching requests
    }

initialProfileManagerState :: ProfileManagerState
initialProfileManagerState = ProfileManagerState
    { lastFetchAttempts = Map.empty
    , pendingFetches = Set.empty
    , fetchQueue = undefined  -- Will be initialized in runProfileManager
    , activeFetches = undefined  -- Will be initialized in runProfileManager
    , batchedRequests = undefined  -- Will be initialized in runProfileManager
    , batchTimer = undefined  -- Will be initialized in runProfileManager
    }

data ProfileManager :: Effect where
    InitializeProfileManager :: ProfileManager m ()
    GetProfile :: PubKeyXO -> ProfileManager m Profile
    FetchProfile :: PubKeyXO -> [RelayURI] -> ProfileManager m Profile

type instance DispatchOf ProfileManager = Dynamic


initializeProfileManager :: ProfileManager :> es => Eff es ()
initializeProfileManager = send $ InitializeProfileManager

getProfile :: ProfileManager :> es => PubKeyXO -> Eff es Profile
getProfile pk = send $ GetProfile pk

fetchProfile :: ProfileManager :> es => PubKeyXO -> [RelayURI] -> Eff es Profile
fetchProfile pk relayHints = send $ FetchProfile pk relayHints


type ProfileManagerEff es =
    ( LmdbStore :> es
    , RelayConnection :> es
    , KeyMgmt :> es
    , Nostr :> es
    , Util :> es
    , Logging :> es
    , State AppState :> es
    , QtQuick :> es
    , State ProfileManagerState :> es
    , State RelayPool :> es
    , State QtQuickState :> es
    , Concurrent :> es
    , IOE :> es
    )

runProfileManager :: ProfileManagerEff es => Eff (ProfileManager : es) a -> Eff es a
runProfileManager = interpret $ \_ -> \case
    InitializeProfileManager -> do
        queue <- newTChanIO
        activeFetches <- newTVarIO 0
        batchedRequests <- newTVarIO Map.empty
        batchTimer <- newTVarIO Nothing
        put @ProfileManagerState $ ProfileManagerState
            { lastFetchAttempts = Map.empty
            , pendingFetches = Set.empty
            , fetchQueue = queue
            , activeFetches = activeFetches
            , batchedRequests = batchedRequests
            , batchTimer = batchTimer
            }
    GetProfile pk -> fetchProfileImpl pk []
    FetchProfile pk relayHints -> fetchProfileImpl pk relayHints
    

-- | Main profile fetching implementation with queue and batching
fetchProfileImpl :: ProfileManagerEff es => PubKeyXO -> [RelayURI] -> Eff es Profile
fetchProfileImpl pk relayHints = do
    profileCache' <- gets @AppState profileCache
    case Map.lookup pk profileCache' of
        Just profile -> return profile
        Nothing -> do
            profile <- Lmdb.getProfile pk
            profileTimestamp <- Lmdb.getLatestTimestamp pk [Metadata]
            currentTime <- getCurrentTime

            st <- get @ProfileManagerState
            let profileTs = fromMaybe 0 profileTimestamp
                lastFetchAttempt = fromMaybe 0 $ Map.lookup pk (lastFetchAttempts st)
                timeSinceLastFetch = currentTime - lastFetchAttempt
                shouldFetch = profileTs == 0 || timeSinceLastFetch > 24 * 60 * 60
            
            when shouldFetch $ do
                let request = ProfileFetchRequest pk relayHints
                atomically $ writeTChan (fetchQueue st) request
                startQueueProcessor

            modify @AppState $ \s -> s { profileCache = Map.insert pk profile (profileCache s) }
            return profile

-- | Start the queue processor if not already running
startQueueProcessor :: ProfileManagerEff es => Eff es ()
startQueueProcessor = do
    st <- get @ProfileManagerState
    activeCount <- atomically $ readTVar (activeFetches st)
    when (activeCount == 0) $ do
        void $ async processQueue

-- | Process the fetch queue with batching and concurrency control
processQueue :: ProfileManagerEff es => Eff es ()
processQueue = do
    st <- get @ProfileManagerState
    
    -- Dequeue ALL available requests at once
    allRequests <- atomically $ do
        let collectAll acc = do
                result <- tryReadTChan (fetchQueue st)
                case result of
                    Nothing -> return (reverse acc)
                    Just req -> collectAll (req : acc)
        collectAll []
    
    when (not $ null allRequests) $ do
        -- Group requests by relay hints for efficient batching
        let (withHints, withoutHints) = partition (not . null . requestRelayHints) allRequests
        groupedRequests <- groupRequestsByRelay withHints withoutHints
        
        -- Process each relay group concurrently
        forM_ (Map.toList groupedRequests) $ \(relayUri, pubkeys) -> do
            void $ async $ processRelayBatch relayUri pubkeys

-- | Group requests by relay for efficient batching
groupRequestsByRelay :: ProfileManagerEff es => [ProfileFetchRequest] -> [ProfileFetchRequest] -> Eff es (Map.Map RelayURI [PubKeyXO])
groupRequestsByRelay withHints withoutHints = do
    let hintGroups = Map.fromListWith (++) $ concatMap (\req -> 
            map (\hint -> (hint, [requestPubKey req])) (requestRelayHints req)) withHints
    
    -- For requests without hints, we'll distribute them across available relays
    let noHintKeys = map requestPubKey withoutHints
    
    -- Get available relays from the relay pool
    relayPoolSt <- get @RelayPool
    let connectedRelays = Map.keys $ Map.filter (\rd -> connectionState rd == Connected) $ activeConnections relayPoolSt
        allRelays = if null (Map.keys hintGroups) then connectedRelays else Map.keys hintGroups
    
    -- Add no-hint keys to the first few relays (round-robin distribution)
    let distributedGroups = if null allRelays
            then Map.empty
            else distributeKeys noHintKeys allRelays hintGroups
    
    return distributedGroups

-- | Distribute keys across relays in round-robin fashion
distributeKeys :: [PubKeyXO] -> [RelayURI] -> Map.Map RelayURI [PubKeyXO] -> Map.Map RelayURI [PubKeyXO]
distributeKeys keys relays groups = 
    foldl (\acc (i, key) -> 
        let relay = relays !! (i `mod` length relays)
        in Map.insertWith (++) relay [key] acc) groups (zip [0..] keys)

-- | Process a batch of pubkeys for a specific relay
processRelayBatch :: ProfileManagerEff es => RelayURI -> [PubKeyXO] -> Eff es ()
processRelayBatch relayUri pubkeys = do
    st <- get @ProfileManagerState
    
    -- Check if we can start a new fetch (max 3 concurrent)
    canStart <- atomically $ do
        active <- readTVar (activeFetches st)
        if active < 3
            then do
                writeTVar (activeFetches st) (active + 1)
                return True
            else return False
    
    when canStart $ do
        -- Connect to relay
        connected <- connect relayUri
        when connected $ do
            -- Create batched subscription for all pubkeys
            sid <- subscribeTemporary relayUri (metadataFilter pubkeys) handleEvent
            waitForCompletion sid
            
            -- Mark all pubkeys as fetched and complete all requests
            currentTime <- getCurrentTime
            modify @ProfileManagerState $ \s -> s
                { lastFetchAttempts = foldr (\pk acc -> Map.insert pk currentTime acc) (lastFetchAttempts s) pubkeys
                , pendingFetches = foldr Set.delete (pendingFetches s) pubkeys
                }

            -- Add profiles to cache
            forM_ pubkeys $ \pk -> do
                profile <- Lmdb.getProfile pk
                modify @AppState $ \s -> s { profileCache = Map.insert pk profile (profileCache s) }
        
        -- Decrement active fetches counter
        atomically $ do
            active <- readTVar (activeFetches st)
            writeTVar (activeFetches st) (active - 1)
