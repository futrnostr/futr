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

import Nostr
import KeyMgmt (KeyMgmt)
import Nostr.Event (Kind(..))
import Nostr.EventProcessor (handleEvent)
import Nostr.Keys (PubKeyXO)
import Nostr.Profile (Profile)
import Nostr.Relay ( ConnectionState(..), RelayConnection, RelayData(..), RelayPool(..)
                   , SubscriptionState(..), connect, subscribeTemporary )
import Nostr.Types (Filter(..), RelayURI, metadataFilter)
import Nostr.Util
import QtQuick (QtQuick)
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
    GetProfile :: PubKeyXO -> ProfileManager m Profile
    FetchProfile :: PubKeyXO -> [RelayURI] -> ProfileManager m Profile

type instance DispatchOf ProfileManager = Dynamic


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
    , State AppState :> es
    , QtQuick :> es
    , State ProfileManagerState :> es
    , State RelayPool :> es
    , Concurrent :> es
    , IOE :> es
    )

runProfileManager :: ProfileManagerEff es => Eff (ProfileManager : es) a -> Eff es a
runProfileManager = interpret $ \_ -> \case
    GetProfile pk -> fetchProfileImpl pk []
    FetchProfile pk relayHints -> fetchProfileImpl pk relayHints

-- Helper function containing the common implementation
fetchProfileImpl :: forall (es :: [Effect]). ProfileManagerEff es => PubKeyXO -> [RelayURI] -> Eff es Profile
fetchProfileImpl pk relayHints = do
    profile <- Lmdb.getProfile pk
    profileTimestamp <- Lmdb.getLatestTimestamp pk [Metadata]

    void $ async $ do
        currentTime <- getCurrentTime
        st <- get @ProfileManagerState

        relayPoolSt <- get @RelayPool
        let isActiveFetch = any (\sub ->
              let filter' = subscriptionFilter sub
              in pk `elem` fromMaybe [] (authors filter')
                 && maybe False (Metadata `elem`) (kinds filter'))
              (Map.elems $ subscriptions relayPoolSt)
            isPendingFetch = pk `Set.member` pendingFetches st

        let lastFetchAttempt = fromMaybe 0 $ Map.lookup pk (lastFetchAttempts st)
            timeSinceLastFetch = currentTime - lastFetchAttempt
            profileTs = fromMaybe 0 profileTimestamp
            shouldFetch = not (isActiveFetch || isPendingFetch) &&
                         (profileTs == 0 || timeSinceLastFetch > 24 * 60 * 60)

        when shouldFetch $ do
            modify @ProfileManagerState $ \s -> s
                { lastFetchAttempts = Map.insert pk currentTime (lastFetchAttempts s)
                , pendingFetches = Set.insert pk (pendingFetches s)
                }

            let connectedRelays = Map.keys $ Map.filter (\rd -> connectionState rd == Connected) $ activeConnections relayPoolSt
            let targetRelays = if null relayHints then connectedRelays else relayHints

            forM_ targetRelays $ \relayUri -> async $ do
                connected <- connect relayUri
                when connected $ do
                    void $ subscribeTemporary relayUri (metadataFilter [pk]) handleEvent

            modify @ProfileManagerState $ \s -> s
                { pendingFetches = Set.delete pk (pendingFetches s) }

    return profile
