{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Store.Lmdb where

import Control.Monad (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Set qualified as Set
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception (throwIO)
import Effectful.State.Static.Shared (State, get)
import Effectful.State.Static.Shared qualified as State
import Effectful.FileSystem
import Effectful.TH (makeEffect)
import Lmdb.Codec qualified as Codec
import Lmdb.Connection ( initializeReadWriteEnvironment, makeMultiSettings, makeSettings
                       , openMultiDatabase, openDatabase, readonly, withMultiCursor )
import Lmdb.Connection qualified as Connection
import Lmdb.Map qualified as Map
import Lmdb.Multimap qualified as Multimap
import Lmdb.Types
import Pipes.Prelude qualified as Pipes
import System.FilePath ((</>))

import Logging
import Nostr.Keys (PubKeyXO)
import Nostr.Types (Event(..), EventId(..), Profile, emptyProfile)
import Types (AppState(..), EventWithRelays(..), Follow)


-- | Timeline types
data TimelineType = PostTimeline | ChatTimeline
    deriving (Show, Eq)

-- | Timeline key type
type TimelineKey = (PubKeyXO, Int)


-- | Lmdb Store effect
data LmdbStore :: Effect where
    -- Transaction wrapper
    WithTransaction :: forall m a. 
                      (forall es. (LmdbStore :> es, IOE :> es, State AppState :> es) => 
                       Transaction 'ReadWrite -> Eff es a) 
                      -> LmdbStore m a
    
    -- Event operations
    PutEventTx :: Transaction 'ReadWrite -> EventWithRelays -> LmdbStore m ()
    GetEvent :: EventId -> LmdbStore m (Maybe EventWithRelays)
    DeleteEventTx :: Transaction 'ReadWrite -> EventId -> LmdbStore m ()
    
    -- Follow operations
    PutFollowsTx :: Transaction 'ReadWrite -> PubKeyXO -> [Follow] -> LmdbStore m ()
    GetFollows :: PubKeyXO -> LmdbStore m [Follow]
    DeleteFollowsTx :: Transaction 'ReadWrite -> PubKeyXO -> LmdbStore m ()
    
    -- Profile operations
    PutProfileTx :: Transaction 'ReadWrite -> PubKeyXO -> (Profile, Int) -> LmdbStore m ()
    GetProfile :: PubKeyXO -> LmdbStore m (Profile, Int)
    
    -- Timeline operations
    AddTimelineEntryTx :: Transaction 'ReadWrite -> TimelineType -> PubKeyXO -> EventId -> Int -> LmdbStore m ()
    DeleteTimelineEntryTx :: Transaction 'ReadWrite -> TimelineType -> EventId -> LmdbStore m ()
    GetTimelineIds :: TimelineType -> PubKeyXO -> Int -> LmdbStore m [EventId]

type instance DispatchOf LmdbStore = Dynamic

makeEffect ''LmdbStore


-- | Run LmdbStore operations
runLmdbStore :: (IOE :> es, State AppState :> es)
              => Eff (LmdbStore : es) a
              -> Eff es a
runLmdbStore = interpret $ \_ -> \case
    -- Transaction wrapper
    WithTransaction action -> do
        st <- get
        case lmdbEnv st of
            Just env -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE ->
                Connection.withTransaction env $ \txn -> 
                    runE $ inject $ runLmdbStore (action txn)
            _ -> throwIO $ userError "LMDB environment not initialized"

    -- Event operations
    PutEventTx txn ev -> do
        st <- get
        case eventDb st of
            Just db -> do
                let readTxn = readonly txn
                    eid = eventId $ event ev
                existing <- liftIO $ Map.lookup' readTxn db eid
                let mergedEvent = case existing of
                        Just existingEv -> 
                            ev { relays = Set.union (relays ev) (relays existingEv) }
                        Nothing -> ev
                liftIO $ Map.insert' txn db eid mergedEvent
            _ -> throwIO $ userError "Event database not initialized"

    GetEvent eid -> do
        st <- get
        case (lmdbEnv st, eventDb st) of
            (Just env, Just db) -> liftIO $ Connection.withTransaction env $ \txn -> do
                let readTxn = readonly txn
                liftIO $ Map.lookup' readTxn db eid
            _ -> throwIO $ userError "Event database not initialized"

    DeleteEventTx txn eid -> do
        st <- get
        case eventDb st of
            Just db -> liftIO $ Map.delete' txn db eid
            _ -> throwIO $ userError "Event database not initialized"

    -- Follow operations
    PutFollowsTx txn pk follows -> do
        st <- get
        case followsDb st of
            Just db -> liftIO $ Map.insert' txn db pk follows
            _ -> throwIO $ userError "Follows database not initialized"

    GetFollows pk -> do
        st <- get
        case (lmdbEnv st, followsDb st) of
            (Just env, Just db) -> liftIO $ Connection.withTransaction env $ \txn -> do
                let readTxn = readonly txn
                mFollows <- liftIO $ Map.lookup' readTxn db pk
                pure $ maybe [] id mFollows
            _ -> throwIO $ userError "Follows database not initialized"

    DeleteFollowsTx txn pk -> do
        st <- get
        case followsDb st of
            Just db -> liftIO $ Map.delete' txn db pk
            _ -> throwIO $ userError "Follows database not initialized"

    -- Profile operations
    PutProfileTx txn pk newProf@(profile, newTimestamp) -> do
        st <- get
        case profileDb st of
            Just db -> do
                let readTxn = readonly txn
                existing <- liftIO $ Map.lookup' readTxn db pk
                let finalProf = case existing of
                        Just (existingProfile, existingTimestamp) -> 
                            if existingTimestamp >= newTimestamp 
                                then (existingProfile, existingTimestamp)
                                else newProf
                        Nothing -> newProf
                liftIO $ Map.insert' txn db pk finalProf
            _ -> throwIO $ userError "Profile database not initialized"

    GetProfile pk -> do
        st <- get
        case (lmdbEnv st, profileDb st) of
            (Just env, Just db) -> liftIO $ Connection.withTransaction env $ \txn -> do
                let readTxn = readonly txn
                mProfile <- liftIO $ Map.lookup' readTxn db pk
                case mProfile of
                    Just profile -> return profile
                    Nothing -> return (emptyProfile, 0)
            _ -> throwIO $ userError "Profile database not initialized"

    -- Timeline operations
    AddTimelineEntryTx txn timelineType author eid timestamp -> do
        st <- get
        let key = (author, timestamp)
        let dbSelector = case timelineType of
                PostTimeline -> postTimelineDb
                ChatTimeline -> chatTimelineDb
        case dbSelector st of
            Just db -> liftIO $ 
                withMultiCursor txn db $ \cursor -> 
                    Multimap.insert cursor key eid
            _ -> throwIO $ userError "Timeline database not initialized"

    DeleteTimelineEntryTx txn timelineType eid -> do
        st <- get
        case eventDb st of
            Just evDb -> do
                let readTxn = readonly txn
                mEvent <- liftIO $ Map.lookup' readTxn evDb eid
                case mEvent of
                    Just ev -> do
                        let key = (pubKey $ event ev, createdAt $ event ev)
                        let dbSelector = case timelineType of
                                PostTimeline -> postTimelineDb
                                ChatTimeline -> chatTimelineDb
                        case dbSelector st of
                            Just db -> liftIO $ 
                                withMultiCursor txn db $ \cursor -> do
                                    void $ Pipes.toListM $ Multimap.lookupValues cursor key
                                    Multimap.deleteValues cursor
                    Nothing -> pure ()
            _ -> throwIO $ userError "Event database not initialized"

    GetTimelineIds timelineType author limit -> do
        st <- get
        let key = (author, maxBound)  -- start from newest
        let dbSelector = case timelineType of
                PostTimeline -> postTimelineDb
                ChatTimeline -> chatTimelineDb
        case (lmdbEnv st, dbSelector st) of
            (Just env, Just db) -> liftIO $ Connection.withTransaction env $ \txn -> do
                let readTxn = readonly txn
                withMultiCursor readTxn db $ \cursor -> do
                    values <- Pipes.toListM $ Multimap.lookupValues cursor key
                    return $ take limit values
            _ -> throwIO $ userError "Timeline database not initialized"


-- | Helper functions for timeline operations (optional convenience wrappers)
addPostTimelineEntryTx :: (LmdbStore :> es) 
                       => Transaction 'ReadWrite -> PubKeyXO -> EventId -> Int -> Eff es ()
addPostTimelineEntryTx txn pk eid ts = addTimelineEntryTx txn PostTimeline pk eid ts

addChatTimelineEntryTx :: (LmdbStore :> es) 
                       => Transaction 'ReadWrite -> PubKeyXO -> EventId -> Int -> Eff es ()
addChatTimelineEntryTx txn pk eid ts = addTimelineEntryTx txn ChatTimeline pk eid ts

deletePostTimelineEntryTx :: (LmdbStore :> es) => Transaction ReadWrite -> EventId -> Eff es ()
deletePostTimelineEntryTx txn eid = 
    deleteTimelineEntryTx txn PostTimeline eid

deleteChatTimelineEntryTx :: (LmdbStore :> es) => Transaction ReadWrite -> EventId -> Eff es ()
deleteChatTimelineEntryTx txn eid = 
    deleteTimelineEntryTx txn ChatTimeline eid


-- | Default Lmdb settings for JSON-serializable types
defaultJsonSettings :: (Ord k, ToJSON k, FromJSON k, ToJSON v, FromJSON v) 
                   => DatabaseSettings k v
defaultJsonSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (LBS.toStrict . encode)
        (decode . LBS.fromStrict))
    (Codec.throughByteString
        (LBS.toStrict . encode)
        (decode . LBS.fromStrict))

-- | Default Lmdb settings for JSON-serializable types in MultiDatabase
defaultMultiJsonSettings :: (Ord k, Ord v, ToJSON k, FromJSON k, ToJSON v, FromJSON v) 
                        => MultiDatabaseSettings k v
defaultMultiJsonSettings = makeMultiSettings
    (SortCustom $ CustomSortSafe compare)
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (LBS.toStrict . encode)
        (decode . LBS.fromStrict))
    (Codec.throughByteString
        (LBS.toStrict . encode)
        (decode . LBS.fromStrict))


-- Lmdb configuration
maxMapSize :: Int
maxMapSize = 500_000_000_000 -- 500 GB

maxReaders :: Int
maxReaders = 120

maxDbs :: Int
maxDbs = 120

-- | Initialize the Lmdb environment
initializeEnv :: FilePath -> IO (Environment ReadWrite)
initializeEnv dbPath = 
    initializeReadWriteEnvironment maxMapSize maxReaders maxDbs dbPath

initFollowsDb :: Transaction 'ReadWrite -> IO (Database PubKeyXO [Follow])
initFollowsDb txn = openDatabase txn (Just "follows") defaultJsonSettings

initProfileDb :: Transaction 'ReadWrite -> IO (Database PubKeyXO (Profile, Int))
initProfileDb txn = openDatabase txn (Just "profiles") defaultJsonSettings

initPostTimelineDb :: Transaction 'ReadWrite -> IO (MultiDatabase TimelineKey EventId)
initPostTimelineDb txn = openMultiDatabase txn (Just "post_timeline") defaultMultiJsonSettings

initChatTimelineDb :: Transaction 'ReadWrite -> IO (MultiDatabase TimelineKey EventId)
initChatTimelineDb txn = openMultiDatabase txn (Just "chat_timeline") defaultMultiJsonSettings

-- | Settings for the event database
eventDbSettings :: DatabaseSettings EventId EventWithRelays
eventDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(EventId bs) -> bs)
        (Just . EventId))
    (Codec.throughByteString
        (\(EventWithRelays ev rs) -> LBS.toStrict $ encode (ev, Set.toList rs))
        (\bs -> case decode (LBS.fromStrict bs) of
            Just (ev, rsList) -> Just $ EventWithRelays ev (Set.fromList rsList)
            Nothing -> error "Failed to decode EventWithRelays"))


-- | Initialize the event database
initEventDb :: Transaction 'ReadWrite -> IO (Database EventId EventWithRelays)
initEventDb txn = openDatabase txn (Just "events") eventDbSettings
