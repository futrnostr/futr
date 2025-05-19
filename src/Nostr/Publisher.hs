module Nostr.Publisher where

import Control.Monad (forM, forM_)
import Data.List (nub, partition)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically, writeTChan)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, modify)

import Logging
import Nostr.Event (Event(..), EventId)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Relay (RelayURI, getUri, isInboxCapable, isOutboxCapable)
import Nostr.RelayConnection
import Nostr.EventHandler (EventHandler, handleEvent)
import Nostr.Types (Request(..))
import Nostr.Util
import QtQuick
import Store.Lmdb (LmdbStore, getFollows, getDMRelays, getGeneralRelays)
import Types ( AppState(..), ConnectionState(..), Follow(..)
             , PublishStatus(..), RelayData(..), RelayPool(..) )


-- | Result of a publish operation
data PublishResult
    = PublishSuccess [RelayURI] [RelayURI]
    | PublishFailure Text
    deriving (Show)


-- | Publisher effects
data Publisher :: Effect where
    Broadcast :: Event -> Publisher m ()
    PublishToOutbox :: Event -> Publisher m ()
    PublishToRelay :: Event -> RelayURI -> Publisher m ()
    PublishGiftWrap :: Event -> PubKeyXO -> PubKeyXO -> Publisher m () -- sender, recipient
    GetPublishResult :: EventId -> Publisher m PublishResult

type instance DispatchOf Publisher = Dynamic


broadcast :: Publisher :> es => Event -> Eff es ()
broadcast event = send $ Broadcast event

publishToOutbox :: Publisher :> es => Event -> Eff es ()
publishToOutbox event = send $ PublishToOutbox event

publishToRelay :: Publisher :> es => Event -> RelayURI -> Eff es ()
publishToRelay event uri = send $ PublishToRelay event uri

publishGiftWrap :: Publisher :> es => Event -> PubKeyXO -> PubKeyXO -> Eff es ()
publishGiftWrap event sender recipient = send $ PublishGiftWrap event sender recipient

getPublishResult :: Publisher :> es => EventId -> Eff es PublishResult
getPublishResult eid = send $ GetPublishResult eid


-- | Publisher effect handlers
type PublisherEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
  , EventHandler :> es
  , RelayConnection :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  , Util :> es
  )


-- | Run a publisher
runPublisher
  :: PublisherEff es
  => Eff (Publisher : es) a
  -> Eff es a
runPublisher =  interpret $ \_ -> \case
    Broadcast event' -> do
        handleEvent Nothing event'

        kp <- getKeyPair
        let xo = keyPairToPubKeyXO kp

        dmRelays <- getDMRelays xo
        myGeneralRelays <- getGeneralRelays xo
        let outboxCapable = filter isOutboxCapable myGeneralRelays

        follows <- getFollows xo
        let followPks = map pubkey follows

        followerRelays <- forM followPks $ \pk -> do
            relays' <- getGeneralRelays pk
            return $ filter isInboxCapable relays'

        let allTargetRelays = nub $
                dmRelays ++
                map getUri outboxCapable ++
                concatMap (map getUri) followerRelays

        initEventPublishStatus (eventId event') allTargetRelays

        existingConnections <- getConnectedRelays
        let (existingRelays, newRelays) = partition (`elem` existingConnections) allTargetRelays

        forM_ existingRelays $ \r -> writeToChannel event' r

        forM_ newRelays $ \r -> async $ do
            connected <- connect r
            if connected
                then do
                    writeToChannel event' r
                    disconnect r
                else
                    updateEventRelayStatus (eventId event') r (Failure "Relay server unreachable")

    PublishToOutbox event' -> do
        handleEvent Nothing event'

        kp <- getKeyPair
        let pk = keyPairToPubKeyXO kp

        generalRelayList <- getGeneralRelays pk
        let outboxCapableURIs = map getUri $ filter isOutboxCapable generalRelayList

        initEventPublishStatus (eventId event') outboxCapableURIs

        forM_ outboxCapableURIs $ \r -> writeToChannel event' r

    PublishToRelay event' relayUri' -> do
        handleEvent Nothing event'
        updateEventRelayStatus (eventId event') relayUri' Publishing
        writeToChannel event' relayUri'

    PublishGiftWrap event' senderPk recipientPk -> do
        handleEvent Nothing event'
        dmRelayList <- getDMRelays senderPk
        recipientDMRelays <- getDMRelays recipientPk

        if null dmRelayList || null recipientDMRelays
            then pure ()
            else do
                let allRelayURIs = nub $ dmRelayList ++ recipientDMRelays

                initEventPublishStatus (eventId event') allRelayURIs

                existingConnections <- getConnectedRelays
                let (existingRelays, newRelays) = partition
                        (`elem` existingConnections)
                        allRelayURIs

                forM_ existingRelays $ \r -> writeToChannel event' r
                forM_ newRelays $ \r -> async $ do
                    connected <- connect r
                    if connected
                        then do
                            writeToChannel event' r
                            -- Delay disconnect to allow message transmission
                            threadDelay 1000000  -- 1 second delay
                            disconnect r
                        else
                            updateEventRelayStatus (eventId event') r (Failure "Relay server unreachable")

    GetPublishResult eventId' -> do
        st <- get @RelayPool
        let states = Map.findWithDefault Map.empty eventId' (publishStatus st)
        if null states
            then return $ PublishFailure "No relays found to publish to"
            else do
                let (successRelays, failureRelays) = Map.partitionWithKey (\_ v -> v == Success) states
                return $ PublishSuccess (Map.keys successRelays) (Map.keys failureRelays)


-- | Write an event to a channel
writeToChannel :: PublisherEff es => Event -> RelayURI -> Eff es ()
writeToChannel e r = do
    st <- get @RelayPool
    case Map.lookup r (activeConnections st) of
        Just rd -> do
            atomically $ writeTChan (requestChannel rd) (SendEvent e)
            updateEventRelayStatus (eventId e) r WaitingForConfirmation
        Nothing ->
            updateEventRelayStatus (eventId e) r (Failure "No active connection")


-- | Get the connected relays
getConnectedRelays :: PublisherEff es => Eff es [RelayURI]
getConnectedRelays = do
    st <- get @RelayPool
    return $ Map.keys $ Map.filter ((== Connected) . connectionState) (activeConnections st)


-- | Update publish status for an event at a specific relay
updateEventRelayStatus :: PublisherEff es => EventId -> RelayURI -> PublishStatus -> Eff es ()
updateEventRelayStatus eid relay status =
    modify $ \st -> st
        { publishStatus = Map.adjust
            (Map.insert relay status)
            eid
            (publishStatus st)
        }


-- | Initialize publish status for an event with multiple relays
initEventPublishStatus :: PublisherEff es => EventId -> [RelayURI] -> Eff es ()
initEventPublishStatus eid rs =
    modify $ \st -> st
        { publishStatus = Map.insert
            eid
            (Map.fromList [(relay, Publishing) | relay <- rs])
            (publishStatus st)
        }
