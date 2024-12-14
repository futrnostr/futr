{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Store.Lmdb where

import Control.Monad (forM_,void)
import Data.ByteString.Lazy qualified as LBS
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Set qualified as Set
import Effectful
import Effectful.Exception (throwIO)
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared (State, get)
import Effectful.State.Static.Shared qualified as State
import Effectful.FileSystem
import Effectful.TH (makeEffect)
import Lmdb.Codec qualified as Codec
import Lmdb.Connection
import Lmdb.Map qualified as Map
import Lmdb.Multimap qualified as Multimap
import Lmdb.Types
import Pipes.Prelude qualified as Pipes
import System.FilePath ((</>))

import Logging
import Nostr.Keys (PubKeyXO)
import Nostr.Types (Event(..), EventId(..), Kind(..),Profile, emptyProfile)
import Types (AppState(..), EventWithRelays(..), Follow)


-- | Timeline types
data TimelineType = PostTimeline | ChatTimeline
    deriving (Show, Eq)

-- | Timeline key type
type TimelineKey = (PubKeyXO, Int)


-- | LmdbStore operations
data LmdbStore :: Effect where
    -- Event operations
    PutEvent :: EventWithRelays -> LmdbStore m ()
    GetEvent :: EventId -> LmdbStore m (Maybe EventWithRelays)
    DeleteEvent :: EventId -> LmdbStore m ()
    
    -- Follow operations
    PutFollows :: EventWithRelays -> [Follow] -> LmdbStore m ()
    GetFollows :: PubKeyXO -> LmdbStore m [Follow]
    DeleteFollows :: EventId -> PubKeyXO -> LmdbStore m ()
    
    -- Profile operations
    PutProfile :: EventWithRelays -> (Profile, Int) -> LmdbStore m ()
    GetProfile :: PubKeyXO -> LmdbStore m (Profile, Int)
    
    -- Timeline operations
    AddTimelineEntry :: TimelineType -> EventWithRelays -> [PubKeyXO] -> Int -> LmdbStore m ()
    DeleteTimelineEntry :: EventId -> LmdbStore m ()
    GetTimelineIds :: TimelineType -> PubKeyXO -> Int -> LmdbStore m [EventId]


type instance DispatchOf LmdbStore = Dynamic

makeEffect ''LmdbStore


-- | Run LmdbEffect
runLmdbStore :: (IOE :> es, State AppState :> es)
             => Eff (LmdbStore : es) a
             -> Eff es a
runLmdbStore = interpret $ \env -> \case
    -- Event operations
    PutEvent ev -> do
        st <- get
        case (eventDb st, lmdbEnv st) of
            (Just eventDb', Just env') -> liftIO $ withTransaction env' $ \txn ->
                Map.insert' txn eventDb' (eventId $ event ev) ev
            _ -> throwIO $ userError "Event database not initialized"

    GetEvent eid -> do
        st <- get
        case (eventDb st, lmdbEnv st) of
            (Just eventDb', Just env') -> liftIO $ withTransaction env' $ \txn ->
                Map.lookup' (readonly txn) eventDb' eid
            _ -> throwIO $ userError "Event database not initialized"

    DeleteEvent eid -> do
        st <- get
        case (eventDb st, lmdbEnv st) of
            (Just eventDb', Just env') -> liftIO $ withTransaction env' $ \txn ->
                Map.delete' txn eventDb' eid
            _ -> throwIO $ userError "Event database not initialized"

    -- Follow operations
    PutFollows ev follows -> do
        st <- get
        case (eventDb st, followsDb st, lmdbEnv st) of
            (Just eventDb', Just followsDb', Just env') -> liftIO $ withTransaction env' $ \txn -> do
                Map.insert' txn followsDb' (pubKey $ event ev) follows
                Map.insert' txn eventDb' (eventId $ event ev) ev
            _ -> throwIO $ userError "Follows or Event database not initialized"

    GetFollows pk -> do
        st <- get
        case (followsDb st, lmdbEnv st) of
            (Just followsDb', Just env') -> liftIO $ withTransaction env' $ \txn -> do
                mFollows <- Map.lookup' (readonly txn) followsDb' pk
                pure $ maybe [] id mFollows
            _ -> throwIO $ userError "Follows database not initialized"

    DeleteFollows eid pk -> do
        st <- get
        case (eventDb st, followsDb st, lmdbEnv st) of
            (Just eventDb', Just followsDb', Just env') -> liftIO $ withTransaction env' $ \txn -> do
                Map.delete' txn followsDb' pk
                Map.delete' txn eventDb' eid
            _ -> throwIO $ userError "Follows or Event database not initialized"

    -- Profile operations
    PutProfile ev newProf@(profile, newTimestamp) -> do
        st <- get
        case (profileDb st, eventDb st, lmdbEnv st) of
            (Just profileDb', Just eventDb', Just env') -> liftIO $ withTransaction env' $ \txn -> do
                existing <- Map.lookup' (readonly txn) profileDb' (pubKey $ event ev)
                let finalProf = case existing of
                        Just (existingProfile, existingTimestamp) -> 
                            if existingTimestamp >= newTimestamp 
                                then (existingProfile, existingTimestamp)
                                else newProf
                        Nothing -> newProf
                Map.insert' txn profileDb' (pubKey $ event ev) finalProf
                Map.insert' txn eventDb' (eventId $ event ev) ev
            _ -> throwIO $ userError "Profile or Event database not initialized"

    GetProfile pk -> do
        st <- get
        case (lmdbEnv st, profileDb st) of
            (Just env', Just profileDb') -> liftIO $ withTransaction env' $ \txn -> do
                mProfile <- Map.lookup' (readonly txn) profileDb' pk
                case mProfile of
                    Just profile -> return profile
                    Nothing -> return (emptyProfile, 0)
            _ -> throwIO $ userError "Profile database not initialized"

    -- Timeline operations
    AddTimelineEntry timelineType ev pks timestamp -> do
        st <- get
        let dbSelector = case timelineType of
                PostTimeline -> postTimelineDb
                ChatTimeline -> chatTimelineDb
        case (dbSelector st, eventDb st, lmdbEnv st) of
            (Just timelineDb', Just eventDb', Just env') -> liftIO $ withTransaction env' $ \txn -> do
                withMultiCursor txn timelineDb' $ \cursor -> do
                    forM_ pks $ \pk -> Multimap.insert cursor (pk, timestamp) (eventId $ event ev)
                Map.insert' txn eventDb' (eventId $ event ev) ev
            _ -> throwIO $ userError "Timeline or Event database not initialized"

    DeleteTimelineEntry eid -> do
        st <- get
        case (eventDb st, postTimelineDb st, chatTimelineDb st, lmdbEnv st) of
            (Just eventDb', Just postTimelineDb', Just chatTimelineDb', Just env') -> liftIO $ withTransaction env' $ \txn -> do
                mEvent <- Map.lookup' (readonly txn) eventDb' eid
                case mEvent of
                    Just ev -> do
                        let key = (pubKey $ event ev, createdAt $ event ev)
                            db = case kind (event ev) of
                                ShortTextNote -> postTimelineDb'
                                Repost -> postTimelineDb'
                                _ -> chatTimelineDb'
                        withMultiCursor txn db $ \cursor -> do
                            void $ Pipes.toListM $ Multimap.lookupValues cursor key
                            Multimap.deleteValues cursor
                        Map.delete' txn eventDb' eid
                    Nothing -> pure ()
            _ -> throwIO $ userError "Event or Timeline database not initialized"

    GetTimelineIds timelineType author limit -> do
        st <- get
        let key = (author, maxBound)
            dbSelector = case timelineType of
                PostTimeline -> postTimelineDb
                ChatTimeline -> chatTimelineDb
        case (lmdbEnv st, dbSelector st) of
            (Just env', Just timelineDb') -> liftIO $ withTransaction env' $ \txn ->
                withMultiCursor (readonly txn) timelineDb' $ \cursor -> do
                    values <- Pipes.toListM $ Multimap.lookupValues cursor key
                    return $ take limit values
            _ -> throwIO $ userError "Timeline database not initialized"


-- | Helper functions for timeline operations (optional convenience wrappers)
addPostTimelineEntry :: (LmdbStore :> es) => EventWithRelays -> Eff es ()
addPostTimelineEntry ev = addTimelineEntry PostTimeline ev [pubKey $ event ev] (createdAt $ event ev)

addChatTimelineEntry :: (LmdbStore :> es) => EventWithRelays -> [PubKeyXO] -> Int -> Eff es ()
addChatTimelineEntry ev pks ts = addTimelineEntry ChatTimeline ev pks ts


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
