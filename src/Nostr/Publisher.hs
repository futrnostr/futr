module Nostr.Publisher where

import Control.Monad (forM, forM_, void, when)
import Data.List (nub, partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically, writeTChan)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH

import Logging
import Nostr.Event (Event(..), EventId)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Relay (RelayURI, getUri, isInboxCapable, isOutboxCapable)
import Nostr.RelayConnection
import Nostr.Types (Request(..))
import Nostr.Util
import QtQuick
import Store.Lmdb (LmdbStore, getFollows, getDMRelays, getGeneralRelays, putEvent)
import Types ( AppState(..), ConnectionState(..), EventWithRelays(..), Follow(..)
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

makeEffect ''Publisher


-- | Publisher effect handlers
type PublisherEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
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
        void $ putEvent $ EventWithRelays event' Set.empty

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

        modify $ \st' -> st' 
            { publishStatus = Map.insert 
                (eventId event')
                (Map.fromList [(relay, Publishing) | relay <- allTargetRelays]) 
                (publishStatus st') 
            }
        
        existingConnections <- getConnectedRelays
        let (existingRelays, newRelays) = partition (`elem` existingConnections) allTargetRelays
        
        forM_ existingRelays $ \r -> writeToChannel event' r

        forM_ newRelays $ \r -> async $ do
            connected <- connect r
            if connected
                then do
                    writeToChannel event' r
                    disconnect r
                else do
                    modify $ \st' -> st'
                        { publishStatus = Map.adjust
                            (Map.insert r (Failure "Relay server unreachable"))
                            (eventId event')
                            (publishStatus st')
                        }

    PublishToOutbox event' -> do
        void $ putEvent $ EventWithRelays event' Set.empty

        kp <- getKeyPair
        let pk = keyPairToPubKeyXO kp

        generalRelayList <- getGeneralRelays pk
        let outboxCapableURIs = map getUri $ filter isOutboxCapable generalRelayList

        modify $ \st -> st 
            { publishStatus = Map.insert 
                (eventId event') 
                (Map.fromList [(relay, Publishing) | relay <- outboxCapableURIs]) 
                (publishStatus st) 
            }

        forM_ outboxCapableURIs $ \r -> writeToChannel event' r

    PublishToRelay event' relayUri' -> do
        void $ putEvent $ EventWithRelays event' $ Set.empty
        modify $ \st -> st 
            { publishStatus = Map.adjust
                (\existingMap -> Map.insert relayUri' Publishing existingMap)
                (eventId event')
                (publishStatus st)
            }
        writeToChannel event' relayUri'

    PublishGiftWrap event' senderPk recipientPk -> do
        void $ putEvent $ EventWithRelays event' Set.empty
        dmRelayList <- getDMRelays senderPk
        recipientDMRelays <- getDMRelays recipientPk

        if null dmRelayList || null recipientDMRelays
            then pure ()
            else do
                let allRelayURIs = nub $ dmRelayList ++ recipientDMRelays

                modify $ \st -> st
                    { publishStatus = Map.insert
                        (eventId event')
                        (Map.fromList [(relay, Publishing) | relay <- allRelayURIs])
                        (publishStatus st)
                    }

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
                            disconnect r
                        else do
                            modify $ \st' -> st'
                                { publishStatus = Map.adjust
                                    (Map.insert r (Failure "Relay server unreachable"))
                                    (eventId event')
                                    (publishStatus st')
                                }

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
            modify $ \st' -> st' { publishStatus = Map.adjust (Map.insert r WaitingForConfirmation) (eventId e) (publishStatus st') }
        Nothing -> do
            modify $ \st' -> st' { publishStatus = Map.adjust (Map.insert r (Failure "No channel found")) (eventId e) (publishStatus st') }


-- | Get the connected relays
getConnectedRelays :: PublisherEff es => Eff es [RelayURI]
getConnectedRelays = do
    st <- get @RelayPool
    return $ Map.keys $ Map.filter ((== Connected) . connectionState) (activeConnections st)
