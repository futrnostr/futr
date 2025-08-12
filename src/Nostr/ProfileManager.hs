module Nostr.ProfileManager where

import Control.Monad (forM_, void, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, modify)

import Nostr.Event (Kind(..))
import Nostr.Keys (PubKeyXO)
import Nostr.Profile (Profile)
import Nostr.Relay (RelayURI)
import Nostr.RelayPool (RelayPool, RelayPoolState(..), RelayData(..), SubscriptionState(..), initialRelayPool)
import Nostr.Subscription (Subscription, metadataFilter, subscribe)
import Nostr.SubscriptionHandler (SubscriptionHandler, handleSubscriptionUntilEOSE)
import Nostr.Types (Filter(..))
import Nostr.Util
import Store.Lmdb (LmdbStore)
import Store.Lmdb qualified as Lmdb
import Types

data ProfileManagerState = ProfileManagerState
    { lastFetchAttempts :: Map.Map PubKeyXO Int  -- pubkey -> timestamp of last fetch attempt
    , pendingFetches :: Set.Set PubKeyXO
    }

initialProfileManagerState :: ProfileManagerState
initialProfileManagerState = ProfileManagerState
    { lastFetchAttempts = Map.empty
    , pendingFetches = Set.empty
    }

data ProfileManager :: Effect where
    GetProfile :: PubKeyXO -> ProfileManager m (Profile, Int)
    FetchProfile :: PubKeyXO -> [RelayURI] -> ProfileManager m (Profile, Int)

type instance DispatchOf ProfileManager = Dynamic


getProfile :: ProfileManager :> es => PubKeyXO -> Eff es (Profile, Int)
getProfile pk = send $ GetProfile pk

fetchProfile :: ProfileManager :> es => PubKeyXO -> [RelayURI] -> Eff es (Profile, Int)
fetchProfile pk relayHints = send $ FetchProfile pk relayHints


type ProfileManagerEff es =
    ( LmdbStore :> es
    , Subscription :> es
    , SubscriptionHandler :> es
    , Util :> es
    , State ProfileManagerState :> es
    , State RelayPoolState :> es
    , Concurrent :> es
    )

runProfileManager :: ProfileManagerEff es => Eff (ProfileManager : es) a -> Eff es a
runProfileManager = interpret $ \_ -> \case
    GetProfile pk -> fetchProfileImpl pk []
    FetchProfile pk relayHints -> fetchProfileImpl pk relayHints

-- Helper function containing the common implementation
fetchProfileImpl :: ProfileManagerEff es => PubKeyXO -> [RelayURI] -> Eff es (Profile, Int)
fetchProfileImpl pk relayHints = do
    (profile, profileTimestamp) <- Lmdb.getProfile pk

    void $ async $ do
        currentTime <- getCurrentTime
        st <- get @ProfileManagerState

        relayPoolSt <- get @RelayPoolState
        let isActiveFetch = any (\sub ->
              let filter' = subscriptionFilter sub
              in pk `elem` fromMaybe [] (authors filter')
                 && maybe False (Metadata `elem`) (kinds filter'))
              (Map.elems $ subscriptions relayPoolSt)
            isPendingFetch = pk `Set.member` pendingFetches st

        let lastFetchAttempt = fromMaybe 0 $ Map.lookup pk (lastFetchAttempts st)
            timeSinceLastFetch = currentTime - lastFetchAttempt
            shouldFetch = not (isActiveFetch || isPendingFetch) &&
                         (profileTimestamp == 0 || timeSinceLastFetch > 24 * 60 * 60)

        when shouldFetch $ do
            modify @ProfileManagerState $ \s -> s
                { lastFetchAttempts = Map.insert pk currentTime (lastFetchAttempts s)
                , pendingFetches = Set.insert pk (pendingFetches s)
                }

            let connectedRelays = Map.keys $ Map.filter (\rd -> connectionState rd == Connected) $ activeConnections relayPoolSt
            let targetRelays = if null relayHints then connectedRelays else relayHints

            forM_ targetRelays $ \relayUri -> do
                subId' <- subscribe relayUri (metadataFilter [pk])
                void $ handleSubscriptionUntilEOSE subId'

            modify @ProfileManagerState $ \s -> s
                { pendingFetches = Set.delete pk (pendingFetches s) }

    return (profile, profileTimestamp)
