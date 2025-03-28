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
    , recordFailedRelay
    , getEvent
    , getFollows
    , getProfile
    , getTimelineIds
    , getGeneralRelays
    , getDMRelays
    , getLatestTimestamp
    , getCommentIds
    , getFailedRelaysWithinLastNDays
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (forM, forM_, when)
import Data.Aeson (ToJSON, FromJSON, encode, decode, eitherDecode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Cache.LRU qualified as LRU
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set qualified as Set
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
import Pipes.Prelude qualified as Pipes
import Pipes ((>->))

import Logging
import Nostr.Event ( Event(..), EventId(..), Kind(..), Marker(..), Rumor(..), Tag
                   , eventIdFromHex, validateEvent, unwrapGiftWrap, unwrapSeal )
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO, pubKeyXOFromHex)
import Nostr.Profile (Profile, emptyProfile)
import Nostr.Relay (Relay(..), RelayURI, isValidRelayURI)
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
    , commentDb :: Database EventId [EventId]
    , failedRelaysDb :: Database RelayURI Int
    , eventCache :: LRU.LRU EventId EventWithRelays
    , profileCache :: LRU.LRU PubKeyXO Profile
    , followsCache :: LRU.LRU PubKeyXO [Follow]
    , timelineCache :: LRU.LRU (TimelineType, PubKeyXO, Int) [EventId]
    , generalRelaysCache :: LRU.LRU PubKeyXO ([Relay], Int)
    , dmRelaysCache :: LRU.LRU PubKeyXO ([RelayURI], Int)
    , latestTimestampCache :: LRU.LRU (PubKeyXO, Kind) Int
    , commentCache :: LRU.LRU EventId [EventId]
    } deriving (Generic)


-- | LmdbStore operations
data LmdbStore :: Effect where
    -- Event operations
    PutEvent :: EventWithRelays -> LmdbStore m Bool
    RecordFailedRelay:: RelayURI -> LmdbStore m () -- Int: timestamp

    -- Query operations (read-only)
    GetEvent :: EventId -> LmdbStore m (Maybe EventWithRelays)
    GetFollows :: PubKeyXO -> LmdbStore m [Follow]
    GetProfile :: PubKeyXO -> LmdbStore m Profile
    GetTimelineIds :: TimelineType -> PubKeyXO -> Int -> LmdbStore m [EventId]
    GetGeneralRelays :: PubKeyXO -> LmdbStore m [Relay]
    GetDMRelays :: PubKeyXO -> LmdbStore m [RelayURI]
    GetLatestTimestamp :: PubKeyXO -> [Kind] -> LmdbStore m (Maybe Int)
    GetCommentIds :: EventId -> LmdbStore m [EventId]
    GetFailedRelaysWithinLastNDays :: Int -> LmdbStore m [RelayURI] -- Int: number of days to look back


type instance DispatchOf LmdbStore = Dynamic

makeEffect ''LmdbStore


-- | Run LmdbEffect
runLmdbStore :: (Util :> es, IOE :> es, State LmdbState :> es, Logging :> es)
             => Eff (LmdbStore : es) a
             -> Eff es a
runLmdbStore = interpret $ \_ -> \case
    RecordFailedRelay relayUri -> do
        ts <- getCurrentTime
        st <- get @LmdbState
        liftIO $ withTransaction (lmdbEnv st) $ \txn ->
            Map.repsert' txn (failedRelaysDb st) relayUri ts

    PutEvent ev -> do
        let author = pubKey $ event ev
            eventKind = kind $ event ev
            eventTimestamp = createdAt $ event ev

        currentState <- get @LmdbState
        kp <- getKeyPair
        wasUpdated <- withEffToIO (ConcUnlift Persistent Unlimited) $ \runE ->
          liftIO $ withMVar (lmdbLock currentState) $ \_ ->
          withTransaction (lmdbEnv currentState) $ \txn -> do
            Map.repsert' txn (eventDb currentState) (eventId $ event ev) ev

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
                                                let tags' = rumorTags decryptedRumor
                                                    pListPks = getAllPubKeysFromPTags tags'
                                                    participants = if rumorPubKey decryptedRumor == keyPairToPubKeyXO kp
                                                      then sort pListPks
                                                      else filter (/= keyPairToPubKeyXO kp)
                                                           (rumorPubKey decryptedRumor : sort pListPks)

                                                addTimelineEntryTx txn (chatTimelineDb currentState) ev participants (rumorCreatedAt decryptedRumor)
                                                runE $ modify @LmdbState $ \s -> s
                                                    { timelineCache = foldr (\p cache ->
                                                        removeAuthorTimelineEntries ChatTimeline p cache)
                                                        (timelineCache s)
                                                        participants
                                                    }

                                                pure True
                                        _ -> pure False
                                _ -> pure False
                        _ -> pure False

                ShortTextNote -> do
                    let threadRefs = [ (eid, marker)
                                   | ("e":eidHex:rest) <- tags (event ev)
                                   , Just eid <- [eventIdFromHex eidHex]
                                   , let marker = case drop 1 rest of
                                           ("root":_) -> Just Root
                                           ("reply":_) -> Just Reply
                                           _ -> Nothing
                                   ]

                    let isReply = not $ null threadRefs

                    madeChanges <- if isReply
                        then pure False
                        else do
                            addTimelineEntryTx txn (postTimelineDb currentState) ev [author] eventTimestamp
                            runE $ modify @LmdbState $ \s -> s
                                { timelineCache = removeAuthorTimelineEntries PostTimeline author (timelineCache s) }
                            pure True

                    commentChanges <- fmap or $ forM threadRefs $ \(parentId, _) -> do
                        existing <- Map.lookup' (readonly txn) (commentDb currentState) parentId
                        let comments = maybe [] id existing
                            newEventId = eventId $ event ev
                        if newEventId `elem` comments
                            then pure False
                            else do
                                Map.repsert' txn (commentDb currentState) parentId (newEventId : comments)
                                runE $ modify @LmdbState $ \s -> s
                                    { commentCache = fst $ LRU.delete parentId (commentCache s) }
                                pure True

                    pure (madeChanges || commentChanges)

                Repost -> do
                    let mEventTag = [ (eid, fromMaybe "" relay)
                                    | ("e":eidHex:rest) <- tags (event ev)
                                    , Just eid <- [eventIdFromHex eidHex]
                                    , let relay = listToMaybe rest
                                    ]
                        mPubkeyTag = [ pk
                                   | ("p":pkHex:_) <- tags (event ev)
                                   , Just pk <- [pubKeyXOFromHex pkHex] ]
                        mOriginalEvent = eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev)
                    case (mEventTag, mOriginalEvent) of
                        ((_, relay):_, Right originalEvent)
                            -- Validate it's a repost of kind 1 for kind 6
                            | kind (event ev) == Repost && kind originalEvent == ShortTextNote
                            -- Validate pubkey if provided
                            && (null mPubkeyTag || head mPubkeyTag == pubKey originalEvent)
                            && validateEvent originalEvent -> do
                                Map.repsert' txn (eventDb currentState) (eventId originalEvent)
                                    (EventWithRelays originalEvent (Set.singleton relay))
                                addTimelineEntryTx txn (postTimelineDb currentState) ev [author] eventTimestamp
                                runE $ modify @LmdbState $ \s -> s
                                    { timelineCache = removeAuthorTimelineEntries PostTimeline author (timelineCache s) }
                                pure True
                        _ -> pure False

                EventDeletion -> do
                    let eventIdsToDelete =
                          -- Extract event IDs from e-tags
                          [eid | ("e":eidHex:_) <- tags (event ev)
                               , Just eid <- [eventIdFromHex eidHex]]
                          -- -- Extract event IDs from a-tags (replaceable events) - TODO
                          -- ++ [aid | ["a", coordStr] <- tags (event ev)
                          --         , Just aid <- [parseCoordinate coordStr]]
                          -- where
                          --   parseCoordinate :: Text -> Maybe EventId
                          --   parseCoordinate coord = case Text.splitOn ":" coord of
                          --     [kind, pubkey, dTag] -> Nothing  -- TODO: Implement coordinate parsing
                          --     _ -> Nothing
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

                                runE $ modify @LmdbState $ \s -> s
                                    { eventCache = fst $ LRU.delete eid (eventCache s)
                                    , timelineCache =
                                        removeAuthorTimelineEntries PostTimeline author $
                                        removeAuthorTimelineEntries ChatTimeline author (timelineCache s)
                                    }

                                pure result

                            Nothing -> pure False

                    pure $ any id res

                Metadata -> do
                    case eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev) of
                        Right profile -> do
                            existingProfile <- Map.lookup' (readonly txn) (profileDb currentState) author
                            case existingProfile of
                                Just (_, existingTs) ->
                                    if eventTimestamp > existingTs then do
                                        Map.repsert' txn (profileDb currentState) author (profile, eventTimestamp)
                                        updateState
                                        pure True
                                    else pure False
                                Nothing -> do
                                    Map.repsert' txn (profileDb currentState) author (profile, eventTimestamp)
                                    updateState
                                    pure True
                        Left _ -> pure False
                    where
                        updateState = runE $ modify @LmdbState $ \s -> s
                            { profileCache = fst $ LRU.delete author (profileCache s) }

                FollowList -> do
                    let followList' = [ Follow pk petname
                                    | ("p":pkHex:rest) <- tags (event ev)
                                    , Just pk <- [pubKeyXOFromHex pkHex]
                                    , let petname = case drop 1 rest of
                                            (_:name:_) -> Just name  -- Skip relay URL, get petname
                                            _ -> Nothing
                                    ]
                    existingTimestamp' <- Map.lookup' (readonly txn) (latestTimestampDb currentState) (author, eventKind)
                    case existingTimestamp' of
                        Just existingTs ->
                            if eventTimestamp > existingTs then do
                                Map.repsert' txn (followsDb currentState) author followList'
                                updateState
                                pure True
                            else do
                                pure False
                        Nothing -> do
                            Map.repsert' txn (followsDb currentState) author followList'
                            updateState
                            pure True
                    where
                        updateState = runE $  modify @LmdbState $ \s -> s
                            { followsCache = fst $ LRU.delete author (followsCache s) }

                PreferredDMRelays -> do
                    let validRelayTags = [ uri
                                       | ("relay":uri:_) <- tags (event ev)
                                       , isValidRelayURI uri
                                       ]
                    case validRelayTags of
                        [] -> pure False
                        relays' -> do
                            existingRelays <- Map.lookup' (readonly txn) (dmRelaysDb currentState) author
                            case existingRelays of
                                Just (_, existingTs) ->
                                    if eventTimestamp > existingTs then do
                                        Map.repsert' txn (dmRelaysDb currentState) author (relays', eventTimestamp)
                                        updateState
                                        pure True
                                    else pure False
                                Nothing -> do
                                    Map.repsert' txn (dmRelaysDb currentState) author (relays', eventTimestamp)
                                    updateState
                                    pure True
                    where
                        updateState = runE $ modify @LmdbState $ \s -> s
                            { dmRelaysCache = fst $ LRU.delete author (dmRelaysCache s) }

                RelayListMetadata -> do
                    let validRelayTags = [ r
                                       | ("r":uri:rest) <- tags (event ev)
                                       , isValidRelayURI uri
                                       , let r = case rest of
                                                ("write":_) -> OutboxRelay uri
                                                ("read":_) -> InboxRelay uri
                                                _ -> InboxOutboxRelay uri
                                       ]
                    case validRelayTags of
                        [] -> pure False
                        relays -> do
                            existingRelays <- Map.lookup' (readonly txn) (generalRelaysDb currentState) author
                            case existingRelays of
                                Just (_, existingTs) ->
                                    if eventTimestamp > existingTs then do
                                        Map.repsert' txn (generalRelaysDb currentState) author (relays, eventTimestamp)
                                        updateState
                                        pure True
                                    else pure False
                                Nothing -> do
                                    Map.repsert' txn (generalRelaysDb currentState) author (relays, eventTimestamp)
                                    updateState
                                    pure True
                    where
                        updateState = runE $ modify @LmdbState $ \s -> s
                            { generalRelaysCache = fst $ LRU.delete author (generalRelaysCache s) }


                _ -> pure False

        when wasUpdated $ do
            liftIO $ withMVar (lmdbLock currentState) $ \_ -> withTransaction (lmdbEnv currentState) $ \txn -> do
                existingTimestamp <- Map.lookup' (readonly txn) (latestTimestampDb currentState) (author, eventKind)
                case existingTimestamp of
                    Just existingTs ->
                        when (eventTimestamp > existingTs) $
                            Map.repsert' txn (latestTimestampDb currentState) (author, eventKind) eventTimestamp
                    Nothing ->
                        Map.repsert' txn (latestTimestampDb currentState) (author, eventKind) eventTimestamp

            modify @LmdbState $ \s -> s
                { eventCache = fst $ LRU.delete (eventId $ event ev) (eventCache s) }

        pure wasUpdated

    -- Query operations (read-only)

    GetEvent eid -> do
        st <- get @LmdbState
        case LRU.lookup eid (eventCache st) of
            (_, Just ev) -> pure (Just ev)
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
            (_, Just fs) -> pure fs
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
            (_, Just profile) -> pure profile
            (_, Nothing) -> do
                mp <- liftIO $ withTransaction (lmdbEnv st) $ \txn -> do
                    Map.lookup' (readonly txn) (profileDb st) pk
                let (profile, _) = maybe (emptyProfile, 0) id mp
                modify @LmdbState $ \s -> s { profileCache = LRU.insert pk profile $ profileCache s }
                pure profile

    GetTimelineIds timelineType author limit -> do
        st <- get @LmdbState
        let cacheKey = (timelineType, author, limit)
        case LRU.lookup cacheKey (timelineCache st) of
            (_, Just ids) -> do
                pure ids
            (_, Nothing) -> do
                ids <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    withCursor (readonly txn) (if timelineType == PostTimeline then postTimelineDb st else chatTimelineDb st) $ \cursor -> do
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
            (_, Just (relays, _)) -> pure relays
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
            (_, Just (relays, _)) -> pure relays
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
                (_, Just ts) -> return (Just ts)
                (_, Nothing) -> do
                    mts <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                        Map.lookup' (readonly txn) (latestTimestampDb st) key
                    case mts of
                        Just ts -> do
                            modify @LmdbState $ \s -> s { latestTimestampCache = LRU.insert key ts (latestTimestampCache s) }
                            return (Just ts)
                        Nothing -> return Nothing

        let validTimestamps = catMaybes timestamps
        return $ if null validTimestamps then Nothing else Just (maximum validTimestamps)

    GetCommentIds eventId -> do
        st <- get @LmdbState
        case LRU.lookup eventId (commentCache st) of
            (_, Just comments) -> pure comments
            (_, Nothing) -> do
                mComments <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    Map.lookup' (readonly txn) (commentDb st) eventId
                let comments = maybe [] id mComments
                modify @LmdbState $ \s -> s
                    { commentCache = LRU.insert eventId comments $ commentCache s }
                pure comments

    GetFailedRelaysWithinLastNDays days -> do
        st <- get @LmdbState
        now <- getCurrentTime
        let threshold = now - (days * 86400)
        failedUris <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
            withCursor (readonly txn) (failedRelaysDb st) $ \cursor -> do
                pairs <- Pipes.toListM (Map.firstForward cursor)
                forM pairs $ \kv ->
                    let uri = keyValueKey kv
                        ts = keyValueValue kv
                    in if ts >= threshold
                       then return (Just uri)
                       else do
                           -- Clean up old entries
                           Map.delete' txn (failedRelaysDb st) uri
                           return Nothing
        pure $ catMaybes failedUris


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
maxDbs = 10

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


-- | Extract pubkeys from p-tags in the tags list
getAllPubKeysFromPTags :: [Tag] -> [PubKeyXO]
getAllPubKeysFromPTags = concatMap extractPubKey
  where
    extractPubKey ("p":pubkeyHex:_) = case pubKeyXOFromHex pubkeyHex of
        Just pk -> [pk]
        Nothing -> []
    extractPubKey _ = []

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
        commentDb' <- openDatabase txn (Just "comments") defaultJsonSettings
        failedRelaysDb' <- openDatabase txn (Just "failed_relays") defaultJsonSettings

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
            , commentDb = commentDb'
            , failedRelaysDb = failedRelaysDb'
            , eventCache = LRU.newLRU (Just cacheSize)
            , profileCache = LRU.newLRU (Just smallCacheSize)
            , followsCache = LRU.newLRU (Just smallCacheSize)
            , timelineCache = LRU.newLRU (Just cacheSize)
            , generalRelaysCache = LRU.newLRU (Just smallCacheSize)
            , dmRelaysCache = LRU.newLRU (Just smallCacheSize)
            , latestTimestampCache = LRU.newLRU (Just smallCacheSize)
            , commentCache = LRU.newLRU (Just smallCacheSize)
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
    , commentDb = error "LMDB not initialized"
    , failedRelaysDb = error "LMDB not initialized"
    , eventCache = LRU.newLRU (Just cacheSize)
    , profileCache = LRU.newLRU (Just smallCacheSize)
    , followsCache = LRU.newLRU (Just smallCacheSize)
    , timelineCache = LRU.newLRU (Just cacheSize)
    , generalRelaysCache = LRU.newLRU (Just smallCacheSize)
    , dmRelaysCache = LRU.newLRU (Just smallCacheSize)
    , latestTimestampCache = LRU.newLRU (Just smallCacheSize)
    , commentCache = LRU.newLRU (Just smallCacheSize)
    }

-- | Remove timeline entries for a given author
removeAuthorTimelineEntries :: TimelineType -> PubKeyXO -> LRU.LRU (TimelineType, PubKeyXO, Int) [EventId] -> LRU.LRU (TimelineType, PubKeyXO, Int) [EventId]
removeAuthorTimelineEntries timelineType author cache =
    let entries = LRU.toList cache
        newEntries = [(k, v) | (k@(tt, pk, _), v) <- entries, tt /= timelineType || pk /= author]
    in case LRU.maxSize cache of
        Just maxSize -> LRU.fromList (Just maxSize) newEntries
        Nothing -> LRU.fromList Nothing newEntries
