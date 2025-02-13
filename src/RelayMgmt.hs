{-# LANGUAGE BlockArguments #-}

module RelayMgmt where

import Control.Monad (when)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH

import QtQuick
import Logging
import Nostr
import Nostr.Event (createPreferredDMRelaysEvent, createRelayListMetadataEvent)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Publisher
import Nostr.Relay (Relay(..), RelayURI, defaultDMRelays, defaultGeneralRelays, getUri)
import Nostr.RelayConnection
import Nostr.Util
import Store.Lmdb (LmdbStore, getDMRelays, getGeneralRelays)


-- | Effect for handling RelayMgmt operations.
data RelayMgmt :: Effect where
    -- General Relay Management
    AddGeneralRelay :: PubKeyXO -> RelayURI -> Bool -> Bool -> RelayMgmt m Bool
    RemoveGeneralRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()
    SetDefaultGeneralRelays :: RelayMgmt m ()
    -- DM Relay Management
    AddDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m Bool
    RemoveDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()
    SetDefaultDMRelays :: RelayMgmt m ()

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
        disconnect r'
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
        let newRelay = normalizeRelayURI r
        existingRelays <- getDMRelays pk
        if newRelay `elem` existingRelays
            then return False
            else do
                now <- getCurrentTime
                kp <- getKeyPair
                let unsigned = createPreferredDMRelaysEvent (newRelay : existingRelays) pk now
                signed <- signEvent unsigned kp
                case signed of
                    Just signed' -> broadcast signed'
                    Nothing -> logError $ "Failed to sign preferred DM relays event"
                notifyRelayStatus
                return True

    RemoveDMRelay pk r -> do
        let r' = normalizeRelayURI r
        existingRelays <- getDMRelays pk
        let rs = filter (\relay -> relay /= r') existingRelays
        kp <- getKeyPair
        now <- getCurrentTime
        let unsigned = createPreferredDMRelaysEvent rs pk now
        signed <- signEvent unsigned kp
        case signed of
            Just signed' -> broadcast signed'
            Nothing -> logError $ "Failed to sign preferred DM relays event"
        notifyRelayStatus

    SetDefaultGeneralRelays -> do
        logInfo "Setting default general relays..."
        kp <- getKeyPair
        let xo = keyPairToPubKeyXO kp
        now <- getCurrentTime
        let (allRelays, _) = defaultGeneralRelays
        when (null allRelays) $
            logError "No default relays configured"
        shuffledRelays <- shuffleList allRelays
        let relays = case shuffledRelays of
              [] -> error "No default relays available"
              [x] -> [x]
              xs -> take 2 xs
        let unsigned = createRelayListMetadataEvent relays xo now
        signed <- signEvent unsigned kp
        case signed of
            Just event -> do
                broadcast event
                logInfo "Successfully set default general relays"
            Nothing -> 
                logError "Failed to sign relay list metadata event"

    SetDefaultDMRelays -> do
        logInfo "Setting default DM relays..."
        kp <- getKeyPair
        let xo = keyPairToPubKeyXO kp
        now <- getCurrentTime
        let (dmRelays, _) = defaultDMRelays
        when (null dmRelays) $
            logError "No default DM relays configured"
        shuffledRelays <- shuffleList dmRelays
        let relays = case shuffledRelays of
              [] -> error "No default DM relays available"
              [x] -> [x]
              xs -> take 2 xs
        let unsigned = createPreferredDMRelaysEvent relays xo now
        signed <- signEvent unsigned kp
        case signed of
            Just event -> do
                broadcast event
                logInfo "Successfully set default DM relays"
            Nothing -> 
                logError "Failed to sign preferred DM relays event"


-- | Normalize a Relay by normalizing its URI
normalizeRelay :: Relay -> Relay
normalizeRelay relay = case relay of
    InboxRelay uri -> InboxRelay (normalizeRelayURI uri)
    OutboxRelay uri -> OutboxRelay (normalizeRelayURI uri)
    InboxOutboxRelay uri -> InboxOutboxRelay (normalizeRelayURI uri)
