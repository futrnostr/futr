{-# LANGUAGE BlockArguments #-}

module RelayMgmt where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH

import QtQuick
import Logging
import Nostr
import Nostr.Event (createPreferredDMRelaysEvent, createRelayListMetadataEvent)
import Nostr.Keys (PubKeyXO)
import Nostr.Publisher
import Nostr.RelayConnection
import Nostr.Types (Relay(..), RelayURI, getUri)
import Nostr.Util
import Store.Lmdb (LmdbStore, getDMRelays, getGeneralRelays)


-- | Effect for handling RelayMgmt operations.
data RelayMgmt :: Effect where
    -- General Relay Management
    AddGeneralRelay :: PubKeyXO -> RelayURI -> Bool -> Bool -> RelayMgmt m Bool
    RemoveGeneralRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()
    -- DM Relay Management
    AddDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m Bool
    RemoveDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()

type instance DispatchOf RelayMgmt = Dynamic

makeEffect ''RelayMgmt


-- | RelayMgmtEff
type RelayMgmtEff es =
  ( Nostr :> es
  , RelayConnection :> es
  , Publisher :> es
  , Logging :> es
  , QtQuick :> es
  , Util :> es
  , LmdbStore :> es
  )


-- | Handler for relay pool effects.
runRelayMgmt
  :: RelayMgmtEff es
  => Eff (RelayMgmt : es) a
  -> Eff es a
runRelayMgmt = interpret $ \_ -> \case
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
                
                existingRelays <- getGeneralRelays pk
                if relay'' `elem` existingRelays
                    then return False
                    else do
                        let rs = relay'' : existingRelays
                        now <- getCurrentTime
                        kp <- getKeyPair
                        let unsigned = createRelayListMetadataEvent rs pk now
                        signed <- signEvent unsigned kp
                        case signed of
                            Just signed' -> broadcast signed'
                            Nothing -> logError $ "Failed to sign relay list metadata event"
                        notifyRelayStatus
                        return True

    RemoveGeneralRelay pk r -> do
        let r' = normalizeRelayURI r
        disconnectRelay r'
        existingRelays <- getGeneralRelays pk
        let rs = filter (\relay -> getUri relay /= r') existingRelays
        
        kp <- getKeyPair
        now <- getCurrentTime
        let unsigned = createRelayListMetadataEvent rs pk now
        signed <- signEvent unsigned kp
        case signed of
            Just signed' -> broadcast signed'
            Nothing -> logError $ "Failed to sign relay list metadata event"
        notifyRelayStatus

    AddDMRelay pk r -> do
        let newRelay = InboxOutboxRelay $ normalizeRelayURI r
        existingRelays <- getDMRelays pk
        if newRelay `elem` existingRelays
            then return False
            else do
                now <- getCurrentTime
                kp <- getKeyPair
                let unsigned = createPreferredDMRelaysEvent (map getUri $ newRelay : existingRelays) pk now
                signed <- signEvent unsigned kp
                case signed of
                    Just signed' -> broadcast signed'
                    Nothing -> logError $ "Failed to sign preferred DM relays event"
                notifyRelayStatus
                return True

    RemoveDMRelay pk r -> do
        let r' = normalizeRelayURI r
        existingRelays <- getDMRelays pk
        let rs = filter (\relay -> getUri relay /= r') existingRelays
        kp <- getKeyPair
        now <- getCurrentTime
        let unsigned = createPreferredDMRelaysEvent (map getUri rs) pk now
        signed <- signEvent unsigned kp
        case signed of
            Just signed' -> broadcast signed'
            Nothing -> logError $ "Failed to sign preferred DM relays event"
        notifyRelayStatus


-- | Normalize a Relay by normalizing its URI
normalizeRelay :: Relay -> Relay
normalizeRelay relay = case relay of
    InboxRelay uri -> InboxRelay (normalizeRelayURI uri)
    OutboxRelay uri -> OutboxRelay (normalizeRelayURI uri)
    InboxOutboxRelay uri -> InboxOutboxRelay (normalizeRelayURI uri)
