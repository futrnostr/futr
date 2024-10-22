{-# LANGUAGE BlockArguments #-}

module RelayMgmt where

import Control.Monad (forM)
import Data.List (dropWhileEnd)
import Data.Map.Strict qualified as Map
import Data.Text (pack, unpack)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import Network.URI (URI(..), parseURI, uriAuthority, uriPort, uriRegName, uriScheme)

import EffectfulQML
import Logging
import Nostr
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (createPreferredDMRelaysEvent, createRelayListMetadataEvent)
import Nostr.Keys (PubKeyXO)
import Nostr.Publisher
import Nostr.Types (Relay(..), RelayURI, getUri)
import Nostr.Util
import Presentation.KeyMgmt (AccountId(..), KeyMgmt, updateRelays)
import Types (ConnectionState(..), RelayPoolState(..), RelayData(..))


-- | Effect for handling RelayMgmt operations.
data RelayMgmt :: Effect where
    -- General Relay Management
    ImportGeneralRelays :: PubKeyXO -> [Relay] -> Int -> RelayMgmt m ()
    AddGeneralRelay :: PubKeyXO -> RelayURI -> Bool -> Bool -> RelayMgmt m Bool
    RemoveGeneralRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()
    GetGeneralRelays :: PubKeyXO -> RelayMgmt m ([(Relay, ConnectionState)], Int)
    -- DM Relay Management
    ImportDMRelays :: PubKeyXO -> [Relay] -> Int -> RelayMgmt m ()
    AddDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m Bool
    RemoveDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()
    GetDMRelays :: PubKeyXO -> RelayMgmt m ([(Relay, ConnectionState)], Int)

type instance DispatchOf RelayMgmt = Dynamic

makeEffect ''RelayMgmt


-- | RelayMgmtEff
type RelayMgmtEff es =
  ( State RelayPoolState :> es
  , Nostr :> es
  , Publisher :> es
  , KeyMgmt :> es
  , Logging :> es
  , EffectfulQML :> es
  , Util :> es
  )


-- | Handler for relay pool effects.
runRelayMgmt
  :: RelayMgmtEff es
  => Eff (RelayMgmt : es) a
  -> Eff es a
runRelayMgmt = interpret $ \_ -> \case
    ImportGeneralRelays pk rs ts -> do
        let rs' = map normalizeRelay rs
        modify @RelayPoolState $ \st -> do
            case Map.lookup pk (generalRelays st) of
                Nothing -> st { generalRelays = Map.insert pk (rs', ts) (generalRelays st) }
                Just (_, existingTs) -> 
                    if ts > existingTs
                        then st { generalRelays = Map.insert pk (rs', ts) (generalRelays st) }
                        else st
        notifyRelayStatus

    AddGeneralRelay pk relay' r w -> do
        if not r && not w
            then
                return False
            else do
                let relay'' = case (r, w) of
                        (True, True) -> InboxOutboxRelay $ normalizeRelayURI relay'
                        (True, False) -> InboxRelay $ normalizeRelayURI relay'
                        (False, True) -> OutboxRelay $ normalizeRelayURI relay'
                        (False, False) -> error "Unreachable due to guard above"
                kp <- getKeyPair
                st' <- get @RelayPoolState
                let (existingRelays, _) = Map.findWithDefault ([], 0) pk (generalRelays st')
                if relay'' `elem` existingRelays
                    then return False
                    else do
                        let rs = relay'' : existingRelays
                        now <- getCurrentTime
                        modify @RelayPoolState $ \st'' -> st''
                            { generalRelays = Map.insert pk (rs, now) (generalRelays st'') }
                        updateRelays (AccountId $ pubKeyXOToBech32 pk) (rs, now)
                        notifyRelayStatus
                        let unsigned = createRelayListMetadataEvent rs pk now
                        signed <- signEvent unsigned kp
                        case signed of
                            Just signed' -> broadcast signed'
                            Nothing -> logError $ "Failed to sign relay list metadata event"
                        return True

    RemoveGeneralRelay pk r -> do
        let r' = normalizeRelayURI r
        modify $ \st -> st 
            { generalRelays = Map.adjust (removeAllRelayTypes r') pk (generalRelays st) }
        updatedRelays <- gets (Map.findWithDefault ([], 0) pk . generalRelays)
        updateRelays (AccountId $ pubKeyXOToBech32 pk) updatedRelays
        notifyRelayStatus
        kp <- getKeyPair
        st <- get @RelayPoolState
        let (rs, _) = Map.findWithDefault ([], 0) pk (generalRelays st)
        now <- getCurrentTime
        let unsigned = createRelayListMetadataEvent rs pk now
        signed <- signEvent unsigned kp
        case signed of
            Just signed' -> broadcast signed'
            Nothing -> logError $ "Failed to sign relay list metadata event"

    GetGeneralRelays pk -> do
        st <- get @RelayPoolState
        let (relays, timestamp) = Map.findWithDefault ([], 0) pk (generalRelays st)
        relaysWithStatus <- forM relays $ \relay -> do
            let uri = getUri relay
            let status = case Map.lookup uri (activeConnections st) of
                    Just rd -> connectionState rd
                    Nothing -> Disconnected
            return (relay, status)
        return (relaysWithStatus, timestamp)

    ImportDMRelays pk rs ts -> do
        let rs' = map normalizeRelay rs
        modify @RelayPoolState $ \st -> do
            case Map.lookup pk (dmRelays st) of
                Nothing -> st { dmRelays = Map.insert pk (rs', ts) (dmRelays st) }
                Just (_, existingTs) -> 
                    if ts > existingTs
                        then st { dmRelays = Map.insert pk (rs', ts) (dmRelays st) }
                        else st
        notifyRelayStatus

    AddDMRelay pk r -> do
        st <- get @RelayPoolState
        let (existingRelays, _) = Map.findWithDefault ([], 0) pk (dmRelays st)
        let newRelay = InboxOutboxRelay $ normalizeRelayURI r
        if newRelay `elem` existingRelays
            then return False
            else do
                now <- getCurrentTime
                modify $ \st' -> st'
                    { dmRelays = Map.insertWith 
                        (\(_, newTime) (oldRelays, _) -> 
                            ([newRelay] ++ oldRelays, newTime))
                        pk 
                        ([newRelay], now) 
                        (dmRelays st') }
                notifyRelayStatus
                kp <- getKeyPair
                st' <- get @RelayPoolState
                let (rs, _) = Map.findWithDefault ([], 0) pk (dmRelays st')
                let unsigned = createPreferredDMRelaysEvent (map getUri rs) pk now
                signed <- signEvent unsigned kp
                case signed of
                    Just signed' -> broadcast signed'
                    Nothing -> logError $ "Failed to sign preferred DM relays event"
                return True

    RemoveDMRelay pk r -> do
        let r' = normalizeRelayURI r
        now <- getCurrentTime
        modify @RelayPoolState $ \st -> st
            { dmRelays = Map.adjust (removeDMRelay' now $ InboxOutboxRelay r') pk (dmRelays st) }
        notifyRelayStatus
        kp <- getKeyPair
        st <- get @RelayPoolState
        let (rs, _) = Map.findWithDefault ([], 0) pk (dmRelays st)
        let unsigned = createPreferredDMRelaysEvent (map getUri rs) pk now
        signed <- signEvent unsigned kp
        case signed of
            Just signed' -> broadcast signed'
            Nothing -> logError $ "Failed to sign preferred DM relays event"
        where
            removeDMRelay' newTime r'' (relays, _) = (filter (/= r'') relays, newTime)

    GetDMRelays pk -> do
        st <- get @RelayPoolState
        let (relays, timestamp) = Map.findWithDefault ([], 0) pk (dmRelays st)
        relaysWithStatus <- forM relays $ \relay -> do
            let uri = getUri relay
            let status = case Map.lookup uri (activeConnections st) of
                    Just rd -> connectionState rd
                    Nothing -> Disconnected
            return (relay, status)
        return (relaysWithStatus, timestamp)


-- | Remove all variants of a relay URI
removeAllRelayTypes :: RelayURI -> ([Relay], Int) -> ([Relay], Int)
removeAllRelayTypes uri (relays, timestamp) = 
    ( filter (\r -> not $ matchesURI r uri) relays
    , timestamp
    )
  where
    matchesURI (InboxRelay u) uri' = u == uri'
    matchesURI (OutboxRelay u) uri' = u == uri'
    matchesURI (InboxOutboxRelay u) uri' = u == uri'


-- | Normalize a relay URI according to RFC 3986
normalizeRelayURI :: RelayURI -> RelayURI
normalizeRelayURI uri = case parseURI (unpack uri) of
    Just uri' -> pack $ 
        (if uriScheme uri' == "wss:" then "wss://" else "ws://") ++
        maybe "" (\auth -> 
            -- Remove default ports
            let hostPort = uriRegName auth ++ 
                    case uriPort auth of
                        ":80" | uriScheme uri' == "ws:" -> ""
                        ":443" | uriScheme uri' == "wss:" -> ""
                        p -> p
            in hostPort
        ) (uriAuthority uri') ++
        -- Remove trailing slash
        dropWhileEnd (== '/') (uriPath uri' ++ uriQuery uri' ++ uriFragment uri')
    Nothing -> uri  -- If parsing fails, return original URI


-- | Normalize a Relay by normalizing its URI
normalizeRelay :: Relay -> Relay
normalizeRelay relay = case relay of
    InboxRelay uri -> InboxRelay (normalizeRelayURI uri)
    OutboxRelay uri -> OutboxRelay (normalizeRelayURI uri)
    InboxOutboxRelay uri -> InboxOutboxRelay (normalizeRelayURI uri)
