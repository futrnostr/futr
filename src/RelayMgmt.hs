{-# LANGUAGE BlockArguments #-}

module RelayMgmt where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)

import QtQuick
import Logging
import Nostr
import Nostr.Event (Kind(..), UnsignedEvent(..), createPreferredDMRelaysEvent)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Publisher
import Nostr.Types ( RelayURI, Relay(..), defaultDMRelays, defaultGeneralRelays, getUri
                   , isInboxCapable, isOutboxCapable, normalizeRelayURI )
import Nostr.Relay
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


addGeneralRelay :: RelayMgmt :> es => PubKeyXO -> RelayURI -> Bool -> Bool -> Eff es Bool
addGeneralRelay pk uri inbox outbox = send $ AddGeneralRelay pk uri inbox outbox

removeGeneralRelay :: RelayMgmt :> es => PubKeyXO -> RelayURI -> Eff es ()
removeGeneralRelay pk uri = send $ RemoveGeneralRelay pk uri

setDefaultGeneralRelays :: RelayMgmt :> es => Eff es ()
setDefaultGeneralRelays = send SetDefaultGeneralRelays

addDMRelay :: RelayMgmt :> es => PubKeyXO -> RelayURI -> Eff es Bool
addDMRelay pk uri = send $ AddDMRelay pk uri

removeDMRelay :: RelayMgmt :> es => PubKeyXO -> RelayURI -> Eff es ()
removeDMRelay pk uri = send $ RemoveDMRelay pk uri

setDefaultDMRelays :: RelayMgmt :> es => Eff es ()
setDefaultDMRelays = send SetDefaultDMRelays


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
                            Nothing -> error "Failed to sign relay list metadata event"
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
            Nothing -> error "Failed to sign relay list metadata event"
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
                    Nothing -> error "Failed to sign preferred DM relays event"
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
            Nothing -> error "Failed to sign preferred DM relays event"
        notifyRelayStatus

    SetDefaultGeneralRelays -> do
        --logInfo "Setting default general relays..."
        kp <- getKeyPair
        let xo = keyPairToPubKeyXO kp
        now <- getCurrentTime
        let (allRelays, _) = defaultGeneralRelays
--        when (null allRelays) $
--            logError "No default relays configured"
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
                --logInfo "Successfully set default general relays"
            Nothing -> pure ()
                --logError "Failed to sign relay list metadata event"

    SetDefaultDMRelays -> do
        --logInfo "Setting default DM relays..."
        kp <- getKeyPair
        let xo = keyPairToPubKeyXO kp
        now <- getCurrentTime
        let (dmRelays, _) = defaultDMRelays
--        when (null dmRelays) $
--            logError "No default DM relays configured"
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
                --logInfo "Successfully set default DM relays"
            Nothing -> pure ()
                --logError "Failed to sign preferred DM relays event"


createRelayListMetadataEvent :: [Relay] -> PubKeyXO -> Int -> UnsignedEvent
createRelayListMetadataEvent relays xo t =
  UnsignedEvent
    { pubKey' = xo
    , createdAt' = t
    , kind' = RelayListMetadata
    , tags' = map makeRelayTag relays
    , content' = ""
    }
  where
    makeRelayTag r = ["r", getUri r] ++
      if isOutboxCapable r && not (isInboxCapable r)
        then ["write"]
      else if isInboxCapable r && not (isOutboxCapable r)
        then ["read"]
      else []
