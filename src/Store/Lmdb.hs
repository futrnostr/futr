{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Store.Lmdb
    ( LmdbStore(..)
    , LmdbState(..)
    , TimelineType(..)
    , initialLmdbState
    , initializeLmdbState
    , runLmdbStore
    , putEvent
    , getEvent
    , getFollows
    , getProfile
    , getTimelineIds
    , getGeneralRelays
    , getDMRelays
    , getLatestTimestamp
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (forM, forM_, when)
import Data.Aeson (ToJSON, FromJSON, encode, decode, eitherDecode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Cache.LRU qualified as LRU
import Data.List (sort)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Lmdb.Codec qualified as Codec
import Lmdb.Connection
import Lmdb.Map qualified as Map
import Lmdb.Types
import Network.URI (URI(..), parseURI, uriAuthority, uriRegName, uriScheme)
import Pipes.Prelude qualified as Pipes
import Pipes ((>->))

import Logging
import Nostr.Event (validateEvent, unwrapGiftWrap, unwrapSeal)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Types ( Event(..), EventId(..), Kind(..), Profile, Relay, RelayURI, Tag(..)
                   , Rumor(..), emptyProfile, getUri, isValidRelayURI
                   , rumorPubKey, rumorTags, rumorCreatedAt )
import Nostr.Util
import Types (EventWithRelays(..), Follow(..))


-- | Timeline types
data TimelineType = PostTimeline | ChatTimeline
    deriving (Show, Eq, Ord)

-- | Timeline key type
type TimelineKey = (PubKeyXO, Int)

-- | LMDB state containing all database handles
data LmdbState = LmdbState
    { lmdbLock :: MVar ()
    , lmdbEnv :: Environment ReadWrite
    , eventDb :: Database EventId EventWithRelays
    , profileDb :: Database PubKeyXO (Profile, Int)
    , postTimelineDb :: Database (PubKeyXO, Int) EventId
    , chatTimelineDb :: Database (PubKeyXO, Int) EventId
    , followsDb :: Database PubKeyXO [Follow]
    , generalRelaysDb :: Database PubKeyXO ([Relay], Int)
    , dmRelaysDb :: Database PubKeyXO ([RelayURI], Int)
    , latestTimestampDb :: Database (PubKeyXO, Kind) Int
    , eventCache :: LRU.LRU EventId EventWithRelays
    , profileCache :: LRU.LRU PubKeyXO (Profile, Int)
    , followsCache :: LRU.LRU PubKeyXO [Follow]
    , timelineCache :: LRU.LRU (TimelineType, PubKeyXO, Int) [EventId]
    , generalRelaysCache :: LRU.LRU PubKeyXO ([Relay], Int)
    , dmRelaysCache :: LRU.LRU PubKeyXO ([RelayURI], Int)
    , latestTimestampCache :: LRU.LRU (PubKeyXO, Kind) Int
    } deriving (Generic)


-- | LmdbStore operations
data LmdbStore :: Effect where
    -- Event operations
    PutEvent :: EventWithRelays -> LmdbStore m Bool
    
    -- Query operations (read-only)
    GetEvent :: EventId -> LmdbStore m (Maybe EventWithRelays)
    GetFollows :: PubKeyXO -> LmdbStore m [Follow]
    GetProfile :: PubKeyXO -> LmdbStore m Profile
    GetTimelineIds :: TimelineType -> PubKeyXO -> Int -> LmdbStore m [EventId]
    GetGeneralRelays :: PubKeyXO -> LmdbStore m [Relay]
    GetDMRelays :: PubKeyXO -> LmdbStore m [RelayURI]
    GetLatestTimestamp :: PubKeyXO -> [Kind] -> LmdbStore m (Maybe Int)


type instance DispatchOf LmdbStore = Dynamic

makeEffect ''LmdbStore


-- | Run LmdbEffect
runLmdbStore :: (Util :> es, IOE :> es, State LmdbState :> es, Logging :> es)
             => Eff (LmdbStore : es) a
             -> Eff es a
runLmdbStore = interpret $ \_ -> \case
    PutEvent ev -> do
        let author = pubKey $ event ev
            eventKind = kind $ event ev
            eventTimestamp = createdAt $ event ev

        currentState <- get @LmdbState
        kp <- getKeyPair
        wasUpdated <- liftIO $ withMVar (lmdbLock currentState) $ \_ -> withTransaction (lmdbEnv currentState) $ \txn -> do
            Map.repsert' txn (eventDb currentState) (eventId $ event ev) ev

            existingTimestamp <- Map.lookup' (readonly txn) (latestTimestampDb currentState) (author, eventKind)
            case existingTimestamp of
                Just existingTs ->
                    when (eventTimestamp > existingTs) $
                        Map.repsert' txn (latestTimestampDb currentState) (author, eventKind) eventTimestamp
                Nothing ->
                    Map.repsert' txn (latestTimestampDb currentState) (author, eventKind) eventTimestamp

            case eventKind of
                GiftWrap -> do
                    mSealedEvent <- unwrapGiftWrap (event ev) kp
                    case mSealedEvent of
                        Just sealedEvent | validateEvent sealedEvent -> 
                            case kind sealedEvent of
                                Seal -> do
                                    mDecryptedRumor <- unwrapSeal sealedEvent kp
                                    case mDecryptedRumor of
                                        Just decryptedRumor
                                            | pubKey sealedEvent == rumorPubKey decryptedRumor -> do
                                                let participants = if rumorPubKey decryptedRumor == keyPairToPubKeyXO kp
                                                      then sort $ getAllPTags (rumorTags decryptedRumor)
                                                      else filter (/= keyPairToPubKeyXO kp)
                                                           (rumorPubKey decryptedRumor : sort (getAllPTags (rumorTags decryptedRumor)))
                                                addTimelineEntryTx txn (chatTimelineDb currentState) ev participants (rumorCreatedAt decryptedRumor)
                                                pure True
                                        _ -> pure False
                                _ -> pure False
                        _ -> pure False

                ShortTextNote -> do
                    addTimelineEntryTx txn (postTimelineDb currentState) ev [author] eventTimestamp
                    pure True

                Repost -> do
                    let etags = [t | t@(ETag _ _ _) <- tags (event ev)]
                        mOriginalEvent = eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev)
                    case (etags, mOriginalEvent) of
                        (ETag _ _ _ : _, Right originalEvent)
                            | validateEvent originalEvent -> do
                                Map.repsert' txn (eventDb currentState) (eventId originalEvent)
                                    (EventWithRelays originalEvent Set.empty)
                                addTimelineEntryTx txn (postTimelineDb currentState) ev [author] eventTimestamp
                                pure True
                        _ -> pure False

                EventDeletion -> do
                    let eventIdsToDelete = [eid | ETag eid _ _ <- tags (event ev)]
                    res <- forM eventIdsToDelete $ \eid -> do
                        mEvent <- Map.lookup' (readonly txn) (eventDb currentState) eid
                        case mEvent of
                            Just deletedEv -> do
                                let key = (pubKey $ event deletedEv, createdAt $ event deletedEv)
                                    timelineDb = case kind (event deletedEv) of
                                        ShortTextNote -> Just $ postTimelineDb currentState
                                        Repost -> Just $ postTimelineDb currentState
                                        GiftWrap -> Just $ chatTimelineDb currentState
                                        _ -> Nothing
                                
                                result <- case timelineDb of
                                    Just db -> do
                                        Map.delete' txn db key
                                        Map.delete' txn (eventDb currentState) eid
                                        pure True
                                    Nothing -> pure False

                                pure result

                            Nothing -> pure False

                    pure $ any id res

                Metadata -> do
                    case eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev) of
                        Right profile -> do
                            Map.repsert' txn (profileDb currentState) author (profile, eventTimestamp)
                            pure True
                        Left _ -> pure False

                FollowList -> do
                    let followList' = [Follow pk petName' | PTag pk _ petName' <- tags (event ev)]
                    existingTimestamp <- Map.lookup' (readonly txn) (latestTimestampDb currentState) (author, eventKind)
                    case existingTimestamp of
                        Just existingTs ->
                            if eventTimestamp > existingTs then do
                                Map.repsert' txn (followsDb currentState) author followList'
                                pure True
                            else pure False
                        Nothing -> do
                            Map.repsert' txn (followsDb currentState) author followList'
                            pure True

                PreferredDMRelays -> do
                    let validRelayTags = [ r' | RelayTag r' <- tags (event ev), isValidRelayURI r' ]
                    case validRelayTags of
                        [] -> pure False
                        relays -> do
                            existingRelays <- Map.lookup' (readonly txn) (dmRelaysDb currentState) author
                            case existingRelays of
                                Just (_, existingTs) ->
                                    if eventTimestamp > existingTs then do
                                        Map.repsert' txn (dmRelaysDb currentState) author (relays, eventTimestamp)
                                        pure True
                                    else pure False
                                Nothing -> do
                                    Map.repsert' txn (dmRelaysDb currentState) author (relays, eventTimestamp)
                                    pure True

                RelayListMetadata -> do
                    let validRelayTags = [ r' | RTag r' <- tags (event ev), isValidRelayURI (getUri r') ]
                    case validRelayTags of
                        [] -> pure False
                        relays -> do
                            existingRelays <- Map.lookup' (readonly txn) (generalRelaysDb currentState) author
                            case existingRelays of
                                Just (_, existingTs) ->
                                    if eventTimestamp > existingTs then do
                                        Map.repsert' txn (generalRelaysDb currentState) author (relays, eventTimestamp)
                                        pure True
                                    else pure False
                                Nothing -> do
                                    Map.repsert' txn (generalRelaysDb currentState) author (relays, eventTimestamp)
                                    pure True

                _ -> pure False

        -- Update caches
        modify @LmdbState $ \s -> s { eventCache = LRU.insert (eventId $ event ev) ev (eventCache s) }

        case eventKind of
            Metadata -> 
                case eitherDecode (fromStrict $ encodeUtf8 $ content (event ev)) of
                    Right profile -> 
                        modify @LmdbState $ \s -> s 
                            { profileCache = LRU.insert author (profile, eventTimestamp) (profileCache s) }
                    Left _ -> 
                        pure ()

            EventDeletion -> 
                let eventIdsToDelete = [eid | ETag eid _ _ <- tags (event ev)]
                in modify @LmdbState $ \s -> s
                    { eventCache = foldr (\eid cache -> fst $ LRU.delete eid cache) (eventCache s) eventIdsToDelete
                    , timelineCache = foldr (\eid cache -> 
                        case LRU.lookup eid (eventCache s) of
                            (_, Just ev') -> 
                                let timelineType = if kind (event ev') `elem` [ShortTextNote, Repost]
                                                    then PostTimeline
                                                    else ChatTimeline
                                    key = (timelineType, pubKey $ event ev', createdAt $ event ev')
                                in fst $ LRU.delete key cache
                            (_, Nothing) -> cache
                        ) (timelineCache s) eventIdsToDelete
                    }

            FollowList ->
                when wasUpdated $
                    let followList' = [Follow pk petName' | PTag pk _ petName' <- tags (event ev)]
                    in modify @LmdbState $ \s -> s
                        { followsCache = LRU.insert author followList' (followsCache s) }

            PreferredDMRelays ->
                when wasUpdated $
                    let validRelays = [ r' | RelayTag r' <- tags (event ev), isValidRelayURI r' ]
                    in modify @LmdbState $ \s -> s
                        { dmRelaysCache = LRU.insert author (validRelays, eventTimestamp) (dmRelaysCache s) }

            RelayListMetadata ->
                when wasUpdated $
                    let validRelays = [ r' | RTag r' <- tags (event ev), isValidRelayURI (getUri r') ]
                    in modify @LmdbState $ \s -> s
                        { generalRelaysCache = LRU.insert author (validRelays, eventTimestamp) (generalRelaysCache s) }

            _ -> pure ()

        pure wasUpdated

    -- Query operations (read-only)

    GetEvent eid -> do
        st <- get @LmdbState
        case LRU.lookup eid (eventCache st) of
            (newCache, Just ev) -> do
                modify @LmdbState $ \s -> s { eventCache = newCache }
                pure (Just ev)
            (_, Nothing) -> do
                mev <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    Map.lookup' (readonly txn) (eventDb st) eid
                case mev of
                    Just ev -> do
                        modify @LmdbState $ \s -> s { eventCache = LRU.insert eid ev $ eventCache s }
                        pure (Just ev)
                    Nothing -> pure Nothing

    GetFollows pk -> do
        st <- get @LmdbState
        case LRU.lookup pk (followsCache st) of
            (newCache, Just fs) -> do
                modify @LmdbState $ \s -> s { followsCache = newCache }
                pure fs
            (_, Nothing) -> do
                mfs <- liftIO $ withTransaction (lmdbEnv st) $ \txn -> do
                    Map.lookup' (readonly txn) (followsDb st) pk
                case mfs of
                    Just follows -> do
                        modify @LmdbState $ \s -> s { followsCache = LRU.insert pk follows $ followsCache s }
                        pure follows
                    Nothing -> pure []

    GetProfile pk -> do
        st <- get @LmdbState
        case LRU.lookup pk (profileCache st) of
            (newCache, Just (profile, _)) -> do
                modify @LmdbState $ \s -> s { profileCache = newCache }
                pure profile
            (_, Nothing) -> do
                mp <- liftIO $ withTransaction (lmdbEnv st) $ \txn -> do
                    Map.lookup' (readonly txn) (profileDb st) pk
                let (profile, _) = maybe (emptyProfile, 0) id mp
                modify @LmdbState $ \s -> s { profileCache = LRU.insert pk (profile, 0) $ profileCache s }
                pure profile

    GetTimelineIds timelineType author limit -> do
        st <- get @LmdbState
        let cacheKey = (timelineType, author, limit)
        case LRU.lookup cacheKey (timelineCache st) of
            (newCache, Just ids) -> do
                modify @LmdbState $ \s -> s { timelineCache = newCache }
                pure ids
            (_, Nothing) -> do
                ids <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    withCursor (readonly txn) (if timelineType == PostTimeline then postTimelineDb st else chatTimelineDb st) $ \cursor ->
                        Pipes.toListM $
                            Map.lastBackward cursor
                            >-> Pipes.filter (\kv -> fst (keyValueKey kv) == author)
                            >-> Pipes.map keyValueValue
                            >-> Pipes.take limit
                modify @LmdbState $ \s -> s { timelineCache = LRU.insert cacheKey ids $ timelineCache s }
                pure ids

    GetGeneralRelays pubKey -> do
        st <- get @LmdbState
        case LRU.lookup pubKey (generalRelaysCache st) of
            (newCache, Just (relays, _)) -> do
                modify @LmdbState $ \s -> s { generalRelaysCache = newCache }
                pure relays
            (_, Nothing) -> do
                mRelays <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    Map.lookup' (readonly txn) (generalRelaysDb st) pubKey
                case mRelays of
                    Just (relaysList, timestamp) -> do
                        modify @LmdbState $ \s -> s { generalRelaysCache = LRU.insert pubKey (relaysList, timestamp) $ generalRelaysCache s }
                        pure relaysList
                    Nothing -> do
                        pure []

    GetDMRelays pubKey -> do
        st <- get @LmdbState
        case LRU.lookup pubKey (dmRelaysCache st) of
            (newCache, Just (relays, _)) -> do
                modify @LmdbState $ \s -> s { dmRelaysCache = newCache }
                pure relays
            (_, Nothing) -> do
                mRelays <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    Map.lookup' (readonly txn) (dmRelaysDb st) pubKey
                case mRelays of
                    Just (relaysList, timestamp) -> do
                        modify @LmdbState $ \s -> s { dmRelaysCache = LRU.insert pubKey (relaysList, timestamp) $ dmRelaysCache s }
                        pure relaysList
                    Nothing -> do
                        pure []

    GetLatestTimestamp pubKey kinds -> do
        st <- get @LmdbState
        timestamps <- forM kinds $ \k -> do
            let key = (pubKey, k)
            case LRU.lookup key (latestTimestampCache st) of
                (newCache, Just ts) -> do
                    modify @LmdbState $ \s -> s { latestTimestampCache = newCache }
                    return (Just ts)
                (_, Nothing) -> do
                    mts <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                        Map.lookup' (readonly txn) (latestTimestampDb st) key
                    case mts of
                        Just ts -> do
                            -- Update the cache
                            modify @LmdbState $ \s -> s { latestTimestampCache = LRU.insert key ts (latestTimestampCache s) }
                            return (Just ts)
                        Nothing -> return Nothing

        let validTimestamps = catMaybes timestamps
        return $ if null validTimestamps then Nothing else Just (maximum validTimestamps)


-- Helper function for timeline entries within a transaction
addTimelineEntryTx :: Transaction 'ReadWrite 
                   -> Database TimelineKey EventId
                   -> EventWithRelays 
                   -> [PubKeyXO] 
                   -> Int 
                   -> IO ()
addTimelineEntryTx txn timelineDb' ev pks timestamp = do
    let invertedTimestamp = maxBound - timestamp
    withCursor txn timelineDb' $ \cursor ->
        forM_ pks $ \pk ->
            Map.repsert cursor (pk, invertedTimestamp) (eventId $ event ev)


-- | Default Lmdb settings for JSON-serializable types with better error handling
defaultJsonSettings :: (Ord k, ToJSON k, FromJSON k, ToJSON v, FromJSON v, Show k, Show v) 
                   => DatabaseSettings k v
defaultJsonSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (toStrict . encode)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right x -> Just x
            Left _ -> Nothing))
    (Codec.throughByteString
        (toStrict . encode)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right x -> Just x
            Left _ -> Nothing))


-- Lmdb configuration
maxMapSize :: Int
maxMapSize = 500_000_000_000 -- 500 GB

maxReaders :: Int
maxReaders = 120

maxDbs :: Int
maxDbs = 8 -- currently 5 are required, leave some room for future growth

-- | Initialize the Lmdb environment
initializeEnv :: FilePath -> IO (Environment ReadWrite, MVar ())
initializeEnv dbPath = do
    env <- initializeReadWriteEnvironment maxMapSize maxReaders maxDbs dbPath
    lock <- newMVar ()
    pure (env, lock)

-- | Settings for the event database
eventDbSettings :: DatabaseSettings EventId EventWithRelays
eventDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(EventId bs) -> bs)
        (Just . EventId))
    (Codec.throughByteString
        (\(EventWithRelays ev rs) -> toStrict $ encode (ev, Set.toList rs))
        (\bs -> case decode (fromStrict bs) of
            Just (ev, rsList) -> Just $ EventWithRelays ev (Set.fromList rsList)
            Nothing -> Nothing))


-- | Settings for the follows database
followsDbSettings :: DatabaseSettings PubKeyXO [Follow]
followsDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\pk -> toStrict $ encode pk)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right pk -> Just pk
            Left _ -> Nothing))
    (Codec.throughByteString
        (\follows -> toStrict $ encode follows)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right follows -> Just follows
            Left _ -> Nothing))


-- | Settings for the latest timestamp database
latestTimestampDbSettings :: DatabaseSettings (PubKeyXO, Kind) Int
latestTimestampDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(pk, k) -> toStrict $ encode (pk, k))
        (\bs -> case eitherDecode (fromStrict bs) of
            Right key -> Just key
            Left _ -> Nothing))
    (Codec.throughByteString
        (toStrict . encode)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right ts -> Just ts
            Left _ -> Nothing))


-- | Get all p tags from the rumor tags
getAllPTags :: [Tag] -> [PubKeyXO]
getAllPTags = mapMaybe extractPubKey
  where
    extractPubKey (PTag pk _ _) = Just pk
    extractPubKey _ = Nothing

-- | Initialize LMDB state
initializeLmdbState :: FilePath -> IO LmdbState
initializeLmdbState dbPath = do
    (env, lock) <- initializeEnv dbPath
    withTransaction env $ \txn -> do
        eventDb' <- openDatabase txn (Just "events") eventDbSettings
        followsDb' <- openDatabase txn (Just "follows") followsDbSettings
        profileDb' <- openDatabase txn (Just "profiles") defaultJsonSettings
        postTimelineDb' <- openDatabase txn (Just "post_timeline") defaultJsonSettings
        chatTimelineDb' <- openDatabase txn (Just "chat_timeline") defaultJsonSettings
        generalRelaysDb' <- openDatabase txn (Just "general_relays") defaultJsonSettings
        dmRelaysDb' <- openDatabase txn (Just "dm_relays") defaultJsonSettings
        latestTimestampDb' <- openDatabase txn (Just "latest_timestamps") latestTimestampDbSettings

        pure $ LmdbState
            { lmdbLock = lock
            , lmdbEnv = env
            , eventDb = eventDb'
            , profileDb = profileDb'
            , postTimelineDb = postTimelineDb'
            , chatTimelineDb = chatTimelineDb'
            , followsDb = followsDb'
            , generalRelaysDb = generalRelaysDb'
            , dmRelaysDb = dmRelaysDb'
            , latestTimestampDb = latestTimestampDb'
            , eventCache = LRU.newLRU (Just cacheSize)
            , profileCache = LRU.newLRU (Just smallCacheSize)
            , followsCache = LRU.newLRU (Just smallCacheSize)
            , timelineCache = LRU.newLRU (Just cacheSize)
            , generalRelaysCache = LRU.newLRU (Just smallCacheSize)
            , dmRelaysCache = LRU.newLRU (Just smallCacheSize)
            , latestTimestampCache = LRU.newLRU (Just smallCacheSize)
            }

-- | Cache size constants
cacheSize :: Integer
cacheSize = 5000

smallCacheSize :: Integer
smallCacheSize = 500

-- | Initial LMDB state before login
initialLmdbState :: LmdbState
initialLmdbState = LmdbState
    { lmdbLock = error "LMDB not initialized"
    , lmdbEnv = error "LMDB not initialized"
    , eventDb = error "LMDB not initialized"
    , profileDb = error "LMDB not initialized"
    , postTimelineDb = error "LMDB not initialized"
    , chatTimelineDb = error "LMDB not initialized"
    , followsDb = error "LMDB not initialized"
    , generalRelaysDb = error "LMDB not initialized"
    , dmRelaysDb = error "LMDB not initialized"
    , latestTimestampDb = error "LMDB not initialized"
    , eventCache = LRU.newLRU (Just cacheSize)
    , profileCache = LRU.newLRU (Just smallCacheSize)
    , followsCache = LRU.newLRU (Just smallCacheSize)
    , timelineCache = LRU.newLRU (Just cacheSize)
    , generalRelaysCache = LRU.newLRU (Just smallCacheSize)
    , dmRelaysCache = LRU.newLRU (Just smallCacheSize)
    , latestTimestampCache = LRU.newLRU (Just smallCacheSize)
    }
