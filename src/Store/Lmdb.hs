{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Store.Lmdb
    ( LmdbStore(..)
    , LmdbState(..)
    , TimelineType(..)
    , PutEventInput(..)
    , PutEventResult(..)
    , DecryptedGiftWrapData(..)
    , initialLmdbState
    , initializeLmdbState
    , runLmdbStore
    , putEvent
    , getEvent
    , getEvents
    , getEventRelays
    , getFollows
    , getProfile
    , getTimelineIds
    , getGeneralRelays
    , getDMRelays
    , getCommentTree
    , getCommentsWithIndentationLevel
    , putRelayStats
    , getRelayStats
    , putFeedAnchor
    , getOrSeedAnchor
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
#ifdef mingw32_HOST_OS
import Control.Monad (forM, forM_, unless, when)
#else
import Control.Monad (forM_, when)
#endif
import Data.Aeson (FromJSON, ToJSON, encode, decode, eitherDecode)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Cache.LRU qualified as LRU
import Data.List (find)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared (State, get, modify)
import GHC.Generics (Generic)
import Lmdb.Codec qualified as Codec
import Lmdb.Connection
import Lmdb.Map qualified as LMap
import Lmdb.Types
import Pipes.Prelude qualified as Pipes
import Pipes ((>->))
import System.Directory (doesFileExist, getFileSize)
import System.FilePath ((</>))

-- Windows specific imports and FFI for marking a file as sparse
#ifdef mingw32_HOST_OS
import Foreign
import System.Win32.Types (HANDLE, BOOL, DWORD, iNVALID_HANDLE_VALUE)
import System.Win32.File (createFile, closeHandle, gENERIC_WRITE, fILE_SHARE_WRITE, oPEN_EXISTING, fILE_ATTRIBUTE_NORMAL)
#endif

import Logging
import Nostr.Event ( Event(..), EventId(..), Kind(..), Marker(..)
                   , eventIdFromHex, validateEvent )
import Nostr.Keys (PubKeyXO, pubKeyXOFromHex)
import Nostr.Profile (Profile, emptyProfile)
import Nostr.Relay (Relay(..), RelayURI, isValidRelayURI, normalizeRelayURI)
import Nostr.Util
import Types (Follow(..))
import Nostr.RelayPool (RelayStats(..))


-- | Timeline types
data TimelineType = PostTimeline | ChatTimeline
    deriving (Show, Eq, Ord)

-- | Timeline key type
type TimelineKey = (PubKeyXO, Int)

-- | LMDB state containing all database handles
data LmdbState = LmdbState
    { lmdbLock :: MVar ()
    , lmdbEnv :: Environment ReadWrite
    , eventDb :: Database EventId Event
    , eventRelaysDb :: Database EventId (Set RelayURI)
    , profileDb :: Database PubKeyXO (Profile, Int)
    , postTimelineDb :: Database (PubKeyXO, Int) EventId
    , chatTimelineDb :: Database (PubKeyXO, Int) EventId
    , followsDb :: Database PubKeyXO [Follow]
    , generalRelaysDb :: Database PubKeyXO ([Relay], Int)
    , dmRelaysDb :: Database PubKeyXO ([RelayURI], Int)
    , commentDb :: Database CommentKey ()
    , relayStatsDb :: Database RelayURI RelayStats
    , feedAnchorsDb :: Database (RelayURI, Text) Int
    , eventCache :: LRU.LRU EventId Event
    , eventRelaysCache :: LRU.LRU EventId (Set RelayURI)
    , profileCache :: LRU.LRU PubKeyXO (Profile, Int)
    , followsCache :: LRU.LRU PubKeyXO [Follow]
    , timelineCache :: LRU.LRU (TimelineType, PubKeyXO, Int) [EventId]
    , generalRelaysCache :: LRU.LRU PubKeyXO ([Relay], Int)
    , dmRelaysCache :: LRU.LRU PubKeyXO ([RelayURI], Int)
    , latestTimestampCache :: LRU.LRU (PubKeyXO, Kind) Int
    , feedAnchorsCache :: LRU.LRU (RelayURI, Text) Int
    , commentCache :: LRU.LRU EventId [CommentTree]
    } deriving (Generic)


-- | Decrypted gift wrap event
data DecryptedGiftWrapData = DecryptedGiftWrapData
    { participants :: [PubKeyXO]
    , rumorTimestamp :: Int
    }


-- | Input for putting an event in the database
data PutEventInput
    = RawEvent Event
    | DecryptedGiftWrapEvent Event DecryptedGiftWrapData


-- | Result of putting an event in the database
data PutEventResult = PutEventResult
    { eventIsNew :: Bool           -- ^ True if the event was new
    , relaysUpdated :: Bool        -- ^ True if relay list was updated
    } deriving (Show, Eq)


-- | Comment key type
type CommentKey = (EventId, EventId, Int, EventId)  -- (rootId, parentId, timestamp, commentId)


-- | Comment tree structure
data CommentTree = CommentTree
    { commentId :: EventId
    , replies :: [CommentTree]
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | LmdbStore operations
data LmdbStore :: Effect where
    -- Event operations
    PutEvent :: PutEventInput -> Maybe RelayURI -> LmdbStore m PutEventResult
    PutRelayStats :: RelayURI -> RelayStats -> LmdbStore m ()
    PutFeedAnchor :: RelayURI -> Text -> Int -> LmdbStore m ()

    -- Query operations (read-only)
    GetEvent :: EventId -> LmdbStore m (Maybe Event)
    GetEvents :: [EventId] -> LmdbStore m [Event]
    GetEventRelays :: EventId -> LmdbStore m (Set RelayURI)
    GetFollows :: PubKeyXO -> LmdbStore m [Follow]
    GetProfile :: PubKeyXO -> LmdbStore m (Profile, Int)
    GetTimelineIds :: TimelineType -> PubKeyXO -> Int -> LmdbStore m [EventId]
    GetGeneralRelays :: PubKeyXO -> LmdbStore m [Relay]
    GetDMRelays :: PubKeyXO -> LmdbStore m [RelayURI]
    GetCommentTree :: EventId -> LmdbStore m (Maybe [CommentTree])
    GetCommentsWithIndentationLevel :: EventId -> LmdbStore m [(EventId, Int)]
    GetRelayStats :: RelayURI -> LmdbStore m (Maybe RelayStats)
    GetOrSeedAnchor :: RelayURI -> Text -> LmdbStore m Int


type instance DispatchOf LmdbStore = Dynamic


-- | Effectful type for LmdbStore.
type LmdbStoreEff es = ( Util :> es
                       , Logging :> es
                       , State LmdbState :> es
                       , IOE :> es )


putEvent :: LmdbStore :> es => PutEventInput -> Maybe RelayURI -> Eff es PutEventResult
putEvent input mUri = send $ PutEvent input mUri


-- | Persist relay stats for a given relay URI
putRelayStats :: LmdbStore :> es => RelayURI -> RelayStats -> Eff es ()
putRelayStats uri stats = send $ PutRelayStats uri stats

-- | Persist a per-relay, per-feed anchor (e.g., last seen boundary)
putFeedAnchor :: LmdbStore :> es => RelayURI -> Text -> Int -> Eff es ()
putFeedAnchor uri feedKey ts = send $ PutFeedAnchor uri feedKey ts

getEvent :: LmdbStore :> es => EventId -> Eff es (Maybe Event)
getEvent eid = send $ GetEvent eid

getEvents :: LmdbStore :> es => [EventId] -> Eff es [Event]
getEvents eids = send $ GetEvents eids

getEventRelays :: LmdbStore :> es => EventId -> Eff es (Set RelayURI)
getEventRelays eid = send $ GetEventRelays eid

getFollows :: LmdbStore :> es => PubKeyXO -> Eff es [Follow]
getFollows pk = send $ GetFollows pk

getProfile :: LmdbStore :> es => PubKeyXO -> Eff es (Profile, Int)
getProfile pk = send $ GetProfile pk

getTimelineIds :: LmdbStore :> es => TimelineType -> PubKeyXO -> Int -> Eff es [EventId]
getTimelineIds timelineType pk limit = send $ GetTimelineIds timelineType pk limit

getGeneralRelays :: LmdbStore :> es => PubKeyXO -> Eff es [Relay]
getGeneralRelays pk = send $ GetGeneralRelays pk

getDMRelays :: LmdbStore :> es => PubKeyXO -> Eff es [RelayURI]
getDMRelays pk = send $ GetDMRelays pk

getCommentTree :: LmdbStore :> es => EventId -> Eff es (Maybe [CommentTree])
getCommentTree eid = send $ GetCommentTree eid

getCommentsWithIndentationLevel :: LmdbStore :> es => EventId -> Eff es [(EventId, Int)]
getCommentsWithIndentationLevel eid = send $ GetCommentsWithIndentationLevel eid


-- | Load persisted relay stats for a given relay URI, if any
getRelayStats :: LmdbStore :> es => RelayURI -> Eff es (Maybe RelayStats)
getRelayStats uri = send $ GetRelayStats uri

-- | Return existing anchor or seed with current time
getOrSeedAnchor :: LmdbStore :> es => RelayURI -> Text -> Eff es Int
getOrSeedAnchor uri key = send $ GetOrSeedAnchor uri key


-- | Run LmdbEffect
runLmdbStore :: LmdbStoreEff es
             => Eff (LmdbStore : es) a
             -> Eff es a
runLmdbStore = interpret $ \_ -> \case

    PutRelayStats uri stats -> do
        st <- get @LmdbState
        liftIO $ withTransaction (lmdbEnv st) $ \txn ->
            LMap.repsert' txn (relayStatsDb st) uri stats

    PutFeedAnchor uri feedKey ts -> do
        st <- get @LmdbState
        liftIO $ withTransaction (lmdbEnv st) $ \txn ->
            LMap.repsert' txn (feedAnchorsDb st) (uri, feedKey) ts
        modify @LmdbState $ \s -> s { feedAnchorsCache = LRU.insert (uri, feedKey) ts (feedAnchorsCache s) }

    PutEvent eventInput mr -> do
        let (event', mDecrypted) = case eventInput of
                RawEvent ev -> (ev, Nothing)
                DecryptedGiftWrapEvent ev dec -> (ev, Just dec)

        let author = pubKey event'
            eventKind = kind event'
            eventTimestamp = createdAt event'
            eventId' = eventId event'

        currentState <- get @LmdbState

        -- Check if the event already exists
        existingEvent <- getEventDirectFromDb eventId'

        result <- withEffToIO (ConcUnlift Persistent Unlimited) $ \runE ->
          liftIO $ withMVar (lmdbLock currentState) $ \_ ->
          withTransaction (lmdbEnv currentState) $ \txn -> do
            let isNewEvent = case existingEvent of
                  Nothing -> True
                  Just _ -> False

            -- Always store the event itself
            when isNewEvent $ do
              LMap.repsert' txn (eventDb currentState) eventId' event'

            -- Update relay list if we have a relay URI
            relaysWereUpdated <- case mr of
              Nothing -> return (False, Set.empty)
              Just uri -> do
                existingRelays <- LMap.lookup' (readonly txn) (eventRelaysDb currentState) eventId'
                let newRelays = Set.singleton uri
                    combinedRelays = case existingRelays of
                      Just existing -> Set.union existing newRelays
                      Nothing -> newRelays
                    hasNewRelays = case existingRelays of
                      Just existing -> not $ existing == combinedRelays
                      Nothing -> True

                when hasNewRelays $ do
                  LMap.repsert' txn (eventRelaysDb currentState) eventId' combinedRelays

                -- Store combinedRelays in the function context for later use with the cache
                return (hasNewRelays, combinedRelays)

            let (hasNewRelays, combinedRelays) = relaysWereUpdated

            -- Process based on event kind if the event is new
            wasProcessed <- if not isNewEvent then return False else case eventKind of
                    GiftWrap -> case mDecrypted of
                        Just decrypted -> do
                            addTimelineEntryTx txn (chatTimelineDb currentState)
                                eventId'
                                (participants decrypted)
                                (rumorTimestamp decrypted)
                            runE $ modify @LmdbState $ \s -> s
                                { timelineCache = foldr (\p cache ->
                                    removeAuthorTimelineEntries ChatTimeline p cache)
                                    (timelineCache s)
                                    (participants decrypted)
                                }
                            return True
                        Nothing -> return False

                    ShortTextNote -> do
                        let threadRefs = [ (eid, marker)
                                    | ("e":eidHex:rest) <- tags event'
                                    , Just eid <- [eventIdFromHex eidHex]
                                    , let marker = case drop 1 rest of
                                            ("root":_) -> Just Root
                                            ("reply":_) -> Just Reply
                                            _ -> Nothing
                                    , isJust marker  -- Only process e tags with markers
                                    ]

                        -- If no thread refs with markers, treat as regular short text note
                        if null threadRefs then do
                            addTimelineEntryTx txn (postTimelineDb currentState) eventId' [author] eventTimestamp
                            runE $ modify @LmdbState $ \s -> s
                                { timelineCache = removeAuthorTimelineEntries PostTimeline author (timelineCache s) }
                        else do
                            -- Find root and reply IDs
                            let rootId = case find (\t -> case t of
                                    ("e":eidHex:rest) -> "root" `elem` rest && isJust (eventIdFromHex eidHex)
                                    _ -> False) (tags event') of
                                    Just ("e":rootHex:_) -> eventIdFromHex rootHex
                                    _ -> Nothing
                                replyId = case find (\t -> case t of
                                    ("e":eidHex:rest) -> "reply" `elem` rest && isJust (eventIdFromHex eidHex)
                                    _ -> False) (tags event') of
                                    Just ("e":replyHex:_) -> eventIdFromHex replyHex
                                    _ -> Nothing

                            -- Only process if we have a root ID (case 1 and 2)
                            case (rootId, replyId) of
                                (Just rootId', Just replyId') -> do
                                    let commentKey = (rootId', replyId', negate eventTimestamp, eventId')
                                    LMap.repsert' txn (commentDb currentState) commentKey ()

                                    let sndCommentKey = (replyId', replyId', negate eventTimestamp, eventId')
                                    LMap.repsert' txn (commentDb currentState) sndCommentKey ()

                                    -- Clean comment cache for both root and reply
                                    runE $ modify @LmdbState $ \s -> s
                                        { commentCache = fst $ LRU.delete replyId' $ fst $ LRU.delete rootId' (commentCache s) }

                                (Just rootId', Nothing) -> do
                                    let commentKey = (rootId', rootId', negate eventTimestamp, eventId')
                                    LMap.repsert' txn (commentDb currentState) commentKey ()

                                    runE $ modify @LmdbState $ \s -> s
                                        { commentCache = fst $ LRU.delete rootId' (commentCache s) }
                                _ -> do -- Invalid event with only reply marker, treat as regular short text note
                                    addTimelineEntryTx txn (postTimelineDb currentState) eventId' [author] eventTimestamp
                                    runE $ modify @LmdbState $ \s -> s
                                        { timelineCache = removeAuthorTimelineEntries PostTimeline author (timelineCache s) }

                        return True

                    Repost -> do
                        let mEventTag = [ (eid, fromMaybe "" relay)
                                        | ("e":eidHex:rest) <- tags event'
                                        , Just eid <- [eventIdFromHex eidHex]
                                        , let relay = listToMaybe rest
                                        ]
                            mPubkeyTag = [ pk
                                    | ("p":pkHex:_) <- tags event'
                                    , Just pk <- [pubKeyXOFromHex pkHex] ]
                            mOriginalEvent = eitherDecode (fromStrict $ encodeUtf8 $ content event')
                        case (mEventTag, mOriginalEvent) of
                            ((_, relayUri'):_, Right originalEvent)
                                -- Validate it's a repost of kind 1 for kind 6
                                | kind event' == Repost && kind originalEvent == ShortTextNote
                                -- Validate pubkey if provided
                                && (null mPubkeyTag || head mPubkeyTag == pubKey originalEvent)
                                && validateEvent originalEvent -> do
                                    -- Store the original event
                                    existing <- LMap.lookup' (readonly txn) (eventDb currentState) (eventId originalEvent)
                                    when (isNothing existing) $ do
                                        LMap.repsert' txn (eventDb currentState) (eventId originalEvent) originalEvent

                                    -- Add relay info for original event
                                    existingRelays <- LMap.lookup' (readonly txn) (eventRelaysDb currentState) (eventId originalEvent)
                                    let relaySet = Set.singleton relayUri'
                                        updatedRelays = maybe relaySet (`Set.union` relaySet) existingRelays
                                    LMap.repsert' txn (eventRelaysDb currentState) (eventId originalEvent) updatedRelays

                                    -- Add to timeline
                                    addTimelineEntryTx txn (postTimelineDb currentState) eventId' [author] eventTimestamp
                                    runE $ modify @LmdbState $ \s -> s
                                        { timelineCache = removeAuthorTimelineEntries PostTimeline author (timelineCache s) }

                                    return True
                            _ -> return False

                    EventDeletion -> do
                        let eventIdsToDelete = [eid | ("e":eidHex:_) <- tags event', Just eid <- [eventIdFromHex eidHex]]
                        forM_ eventIdsToDelete $ \eid -> do
                            mEvent <- LMap.lookup' (readonly txn) (eventDb currentState) eid
                            case mEvent of
                                Just deletedEv -> do
                                    -- Validate that the deletion author matches the event author
                                    if author == pubKey deletedEv then do
                                        let timestamp = createdAt deletedEv
                                            author' = pubKey deletedEv
                                            key = (author', negate timestamp)
                                            timelineDb = case kind deletedEv of
                                                ShortTextNote -> Just $ postTimelineDb currentState
                                                Repost -> Just $ postTimelineDb currentState
                                                GiftWrap -> Just $ chatTimelineDb currentState
                                                _ -> Nothing

                                        case timelineDb of
                                            Just db -> do
                                                LMap.delete' txn db key
                                                LMap.delete' txn (eventDb currentState) eid
                                                LMap.delete' txn (eventRelaysDb currentState) eid
                                            Nothing -> pure ()

                                        runE $ modify @LmdbState $ \s -> s
                                            { eventCache = fst $ LRU.delete eid (eventCache s)
                                            , timelineCache =
                                                removeAuthorTimelineEntries PostTimeline author $
                                                removeAuthorTimelineEntries ChatTimeline author (timelineCache s)
                                            }
                                    else
                                        pure ()

                                Nothing -> pure ()
                        return True

                    Metadata -> do
                        case eitherDecode (fromStrict $ encodeUtf8 $ content event') of
                            Right profile -> do
                                existingProfile <- LMap.lookup' (readonly txn) (profileDb currentState) author
                                case existingProfile of
                                    Just (_, existingTs) ->
                                        if eventTimestamp > existingTs then do
                                            LMap.repsert' txn (profileDb currentState) author (profile, eventTimestamp)
                                            runE $ modify @LmdbState $ \s -> s
                                                { profileCache = fst $ LRU.delete author (profileCache s) }
                                            return True
                                        else
                                            return False
                                    Nothing -> do
                                        LMap.repsert' txn (profileDb currentState) author (profile, eventTimestamp)
                                        runE $ modify @LmdbState $ \s -> s
                                            { profileCache = fst $ LRU.delete author (profileCache s) }
                                        return True
                            Left _ -> return False

                    FollowList -> do
                        let followList' = [ Follow pk petname
                                        | ("p":pkHex:rest) <- tags event'
                                        , Just pk <- [pubKeyXOFromHex pkHex]
                                        , let petname = case drop 1 rest of
                                                (_:name:_) -> Just name  -- Skip relay URL, get petname
                                                _ -> Nothing
                                        ]
                        let existingTimestamp' = Nothing
                        case existingTimestamp' of
                            Just existingTs ->
                                if eventTimestamp > existingTs then do
                                    LMap.repsert' txn (followsDb currentState) author followList'
                                    runE $ modify @LmdbState $ \s -> s
                                        { followsCache = fst $ LRU.delete author (followsCache s) }
                                    return True
                                else
                                    return False
                            Nothing -> do
                                LMap.repsert' txn (followsDb currentState) author followList'
                                runE $ modify @LmdbState $ \s -> s
                                    { followsCache = fst $ LRU.delete author (followsCache s) }
                                return True

                    PreferredDMRelays -> do
                        let validRelayTags = [ normalizeRelayURI uri
                                        | ("relay":uri:_) <- tags event'
                                        , isValidRelayURI uri
                                        ]
                        if null validRelayTags
                          then return False
                          else do
                            let relays' = validRelayTags
                            existingRelays <- LMap.lookup' (readonly txn) (dmRelaysDb currentState) author
                            case existingRelays of
                                Just (_, existingTs) ->
                                    if eventTimestamp > existingTs then do
                                        LMap.repsert' txn (dmRelaysDb currentState) author (relays', eventTimestamp)
                                        runE $ modify @LmdbState $ \s -> s
                                            { dmRelaysCache = fst $ LRU.delete author (dmRelaysCache s) }
                                        return True
                                    else
                                        return False
                                Nothing -> do
                                    LMap.repsert' txn (dmRelaysDb currentState) author (relays', eventTimestamp)
                                    runE $ modify @LmdbState $ \s -> s
                                        { dmRelaysCache = fst $ LRU.delete author (dmRelaysCache s) }
                                    return True

                    RelayListMetadata -> do
                        let validRelayTags = [ r
                                        | ("r":uri:rest) <- tags event'
                                        , isValidRelayURI uri
                                        , let normalizedUri = normalizeRelayURI uri
                                        , let r = case rest of
                                                    ("write":_) -> OutboxRelay normalizedUri
                                                    ("read":_) -> InboxRelay normalizedUri
                                                    _ -> InboxOutboxRelay normalizedUri
                                        ]
                        if null validRelayTags
                          then return False
                          else do
                            let relays = validRelayTags
                            existingRelays <- LMap.lookup' (readonly txn) (generalRelaysDb currentState) author
                            case existingRelays of
                                Just (_, existingTs) ->
                                    if eventTimestamp > existingTs then do
                                        LMap.repsert' txn (generalRelaysDb currentState) author (relays, eventTimestamp)
                                        runE $ modify @LmdbState $ \s -> s
                                            { generalRelaysCache = fst $ LRU.delete author (generalRelaysCache s) }
                                        return True
                                    else
                                        return False
                                Nothing -> do
                                    LMap.repsert' txn (generalRelaysDb currentState) author (relays, eventTimestamp)
                                    runE $ modify @LmdbState $ \s -> s
                                        { generalRelaysCache = fst $ LRU.delete author (generalRelaysCache s) }
                                    return True

                    _ -> return False -- By default, consider other events as not processed

            let result = PutEventResult
                  { eventIsNew = isNewEvent && wasProcessed
                  , relaysUpdated = hasNewRelays
                  }

            -- Update cache
            when ((eventIsNew result) || (relaysUpdated result)) $
                runE $ modify @LmdbState $ \s -> s
                    { eventCache = LRU.insert eventId' event' (eventCache s) }

            when hasNewRelays $
                runE $ modify @LmdbState $ \s -> s
                    { eventRelaysCache = LRU.insert eventId' combinedRelays (eventRelaysCache s) }

            pure result

        pure result

    -- Query operations (read-only)

    GetEvent eid -> do
        st <- get @LmdbState
        case LRU.lookup eid (eventCache st) of
            (newCache, Just event') -> do
                modify @LmdbState $ \s -> s { eventCache = newCache }
                pure (Just event')
            (_, Nothing) -> do
                mev <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    LMap.lookup' (readonly txn) (eventDb st) eid
                case mev of
                    Just event' -> do
                        -- Cache the event
                        modify @LmdbState $ \s -> s { eventCache = LRU.insert eid event' (eventCache s) }
                        pure (Just event')
                    Nothing -> pure Nothing

    GetEvents eids -> do
        st <- get @LmdbState

        let (newCache, cachedEvents) = foldr
                (\eid (cache, events) ->
                    case LRU.lookup eid cache of
                        (newCache', Just event') -> (newCache', event':events)
                        (_, Nothing) -> (cache, events))
                (eventCache st, [])
                eids

        let cachedIds = Set.fromList $ map eventId cachedEvents
            missingIds = filter (\eid -> not $ Set.member eid cachedIds) eids

        missingEvents <- if null missingIds
            then pure []
            else liftIO $ withTransaction (lmdbEnv st) $ \txn -> do
                let sortedIds = Set.toAscList $ Set.fromList missingIds
                    collectRemainingEvents _ acc [] = return acc
                    collectRemainingEvents cursor acc (targetId:rest) = do
                        mNext <- LMap.next cursor
                        case mNext of
                            Nothing -> return acc
                            Just kv -> do
                                let currentId = keyValueKey kv
                                    currentEvent = keyValueValue kv
                                if currentId == targetId
                                    then collectRemainingEvents cursor (currentEvent:acc) rest
                                    else if currentId > targetId
                                            then collectRemainingEvents cursor acc rest
                                            else collectRemainingEvents cursor acc (targetId:rest)

                withCursor txn (eventDb st) $ \cursor -> do
                    mFirst <- LMap.lookupGte cursor (head sortedIds)
                    case mFirst of
                        Nothing -> return []
                        Just kv -> do
                            let firstEvent = keyValueValue kv
                                firstId = keyValueKey kv
                            if firstId == head sortedIds
                                then collectRemainingEvents cursor [firstEvent] (tail sortedIds)
                                else collectRemainingEvents cursor [] sortedIds

        let newEvents = missingEvents
            finalCache = foldr
                (\event cache -> LRU.insert (eventId event) event cache)
                newCache
                newEvents

        modify @LmdbState $ \s -> s { eventCache = finalCache }

        let eventMap = Map.fromList $
                [(eventId e, e) | e <- cachedEvents ++ newEvents]
        return [e | eid <- eids, e <- maybeToList $ Map.lookup eid eventMap]

    GetEventRelays eid -> do
        st <- get @LmdbState
        case LRU.lookup eid (eventRelaysCache st) of
            (newCache, Just relays') -> do
                modify @LmdbState $ \s -> s { eventRelaysCache = newCache }
                pure relays'
            (_, Nothing) -> do
                -- Get relays from DB
                relays' <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    LMap.lookup' (readonly txn) (eventRelaysDb st) eid
                let relaySet = fromMaybe Set.empty relays'
                -- Update cache
                modify @LmdbState $ \s -> s { eventRelaysCache = LRU.insert eid relaySet (eventRelaysCache s) }
                pure relaySet

    GetFollows pk -> do
        st <- get @LmdbState
        case LRU.lookup pk (followsCache st) of
            (_, Just fs) -> pure fs
            (_, Nothing) -> do
                mfs <- liftIO $ withTransaction (lmdbEnv st) $ \txn -> do
                    LMap.lookup' (readonly txn) (followsDb st) pk
                case mfs of
                    Just follows -> do
                        modify @LmdbState $ \s -> s { followsCache = LRU.insert pk follows $ followsCache s }
                        pure follows
                    Nothing -> pure []

    GetProfile pk -> do
        st <- get @LmdbState
        case LRU.lookup pk (profileCache st) of
            (_, Just (profile, timestamp)) -> pure (profile, timestamp)
            (_, Nothing) -> do
                mp <- liftIO $ withTransaction (lmdbEnv st) $ \txn -> do
                    LMap.lookup' (readonly txn) (profileDb st) pk
                let (profile, timestamp) = maybe (emptyProfile, 0) id mp
                modify @LmdbState $ \s -> s { profileCache = LRU.insert pk (profile, timestamp) $ profileCache s }
                pure (profile, timestamp)

    GetTimelineIds timelineType author limit -> do
        st <- get @LmdbState
        let cacheKey = (timelineType, author, limit)
        case LRU.lookup cacheKey (timelineCache st) of
            (_, Just ids) -> do
                pure ids
            (_, Nothing) -> do
                ids <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    withCursor (readonly txn) (if timelineType == PostTimeline then postTimelineDb st else chatTimelineDb st) $ \cursor -> do
                        -- Get posts for this author (newest first since we use negate timestamp)
                        newestPosts <- Pipes.toListM $
                            LMap.lookupGteForward cursor (author, minBound)
                            >-> Pipes.takeWhile (\kv -> fst (keyValueKey kv) == author)
                            >-> Pipes.take limit
                            >-> Pipes.map keyValueValue
                        return newestPosts
                modify @LmdbState $ \s -> s { timelineCache = LRU.insert cacheKey ids $ timelineCache s }
                pure ids

    GetGeneralRelays pubKey -> do
        st <- get @LmdbState
        case LRU.lookup pubKey (generalRelaysCache st) of
            (_, Just (relays, _)) -> pure relays
            (_, Nothing) -> do
                mRelays <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    LMap.lookup' (readonly txn) (generalRelaysDb st) pubKey
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
                    LMap.lookup' (readonly txn) (dmRelaysDb st) pubKey
                case mRelays of
                    Just (relaysList, timestamp) -> do
                        modify @LmdbState $ \s -> s { dmRelaysCache = LRU.insert pubKey (relaysList, timestamp) $ dmRelaysCache s }
                        pure relaysList
                    Nothing -> do
                        pure []

    GetCommentTree eventId -> do
        comments <- getComments eventId
        pure $ Just comments

    GetCommentsWithIndentationLevel eventId -> do
        comments <- getComments eventId
        pure $ flattenComments comments 0

    GetRelayStats uri -> do
        st <- get @LmdbState
        liftIO $ withTransaction (lmdbEnv st) $ \txn ->
            LMap.lookup' (readonly txn) (relayStatsDb st) uri

    GetOrSeedAnchor uri feedKey -> do
        st <- get @LmdbState
        -- fast path via cache
        case LRU.lookup (uri, feedKey) (feedAnchorsCache st) of
          (_, Just ts) -> pure ts
          (_, Nothing) -> do
            mts <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                LMap.lookup' (readonly txn) (feedAnchorsDb st) (uri, feedKey)
            case mts of
              Just ts -> do
                modify @LmdbState $ \s -> s { feedAnchorsCache = LRU.insert (uri, feedKey) ts (feedAnchorsCache s) }
                pure ts
              Nothing -> do
                now <- getCurrentTime
                liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                    LMap.repsert' txn (feedAnchorsDb st) (uri, feedKey) now
                modify @LmdbState $ \s -> s { feedAnchorsCache = LRU.insert (uri, feedKey) now (feedAnchorsCache s) }
                pure now


-- Helper function for timeline entries within a transaction
addTimelineEntryTx :: Transaction 'ReadWrite
                   -> Database TimelineKey EventId
                   -> EventId
                   -> [PubKeyXO]
                   -> Int
                   -> IO ()
addTimelineEntryTx txn timelineDb' eventId pks timestamp = do
    let invertedTimestamp = negate timestamp
    withCursor txn timelineDb' $ \cursor ->
        forM_ pks $ \pk ->
            LMap.repsert cursor (pk, invertedTimestamp) eventId


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


-- Lmdb configuration(

-- | Determine a reasonable map size.
--   Use a conservative approach that grows incrementally:
--     1. Start with current DB size + reasonable headroom (2x current size, min 1GB, max 2GB)
--     2. Map size is fixed per session - app restart will recalculate if DB grows
determineMapSize :: FilePath -> IO Int
determineMapSize path = do
    dbSize <- getDbSize path
    let minSize = 1000 * 1000 * 1000 -- 1 GB minimum
        maxInitialSize = 2 * 1000 * 1000 * 1000 -- 2 GB maximum initial size
        -- Give 2x current size as headroom, but within reasonable bounds
        headroom = max minSize (min maxInitialSize (dbSize * 2))
        mapSize = if dbSize == 0 then minSize else headroom
    pure mapSize


maxReaders :: Int
maxReaders = 120

maxDbs :: Int
maxDbs = 20


-- Windows specific imports and FFI for marking a file as sparse
#ifdef mingw32_HOST_OS
foreign import ccall unsafe "DeviceIoControl"
    c_DeviceIoControl :: HANDLE -> DWORD -> Ptr () -> DWORD -> Ptr () -> DWORD -> Ptr DWORD -> Ptr () -> IO BOOL

fSCTL_SET_SPARSE :: DWORD
fSCTL_SET_SPARSE = 0x900C4

-- Mark a file as sparse (Windows only)
markFileSparse :: FilePath -> IO ()
markFileSparse path = do
    h <- createFile path gENERIC_WRITE fILE_SHARE_WRITE Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
    if h == iNVALID_HANDLE_VALUE
        then pure ()
        else alloca $ \lpBytesReturned -> do
            _ <- c_DeviceIoControl h fSCTL_SET_SPARSE nullPtr 0 nullPtr 0 lpBytesReturned nullPtr
            closeHandle h
            pure ()
#endif

-- | Initialize the Lmdb environment
initializeEnv :: FilePath -> IO (Environment ReadWrite, MVar ())
initializeEnv dbPath = do
#ifdef mingw32_HOST_OS
    let dataFile = dbPath ++ "\\data.mdb"
    exists <- doesFileExist dataFile
    unless exists $ do
        -- Create an empty file so we can mark it as sparse
        writeFile dataFile ""
    markFileSparse dataFile
#endif
    mapSize <- determineMapSize dbPath
    env <- initializeReadWriteEnvironment mapSize maxReaders maxDbs dbPath
    lock <- newMVar ()
    pure (env, lock)


-- Return current size of the LMDB data.mdb file (in bytes) or 0 if absent.
getDbSize :: FilePath -> IO Int
getDbSize dir = do
    let dataFile = dir </> "data.mdb"
    exists <- doesFileExist dataFile
    if exists
        then fmap (fromIntegral :: Integer -> Int) (getFileSize dataFile)
        else pure 0


-- | Settings for the event database
eventDbSettings :: DatabaseSettings EventId Event
eventDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(EventId bs) -> bs)
        (Just . EventId))
    (Codec.throughByteString
        (\ev -> toStrict $ encode ev)
        (\bs -> decode (fromStrict bs)))


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
-- removed latestTimestampDbSettings


-- | Settings for the event relays database
eventRelaysDbSettings :: DatabaseSettings EventId (Set RelayURI)
eventRelaysDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(EventId bs) -> bs)
        (Just . EventId))
    (Codec.throughByteString
        (\relays -> toStrict $ encode (Set.toList relays))
        (\bs -> case decode (fromStrict bs) of
            Just relaysList -> Just $ Set.fromList relaysList
            Nothing -> Nothing))


-- | Settings for the comment database
commentDbSettings :: DatabaseSettings CommentKey ()
commentDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(rootId, parentId, timestamp, commentId) ->
            let rootIdBs = getEventId rootId
                parentIdBs = getEventId parentId
                timestampBs = BS.pack $ encodeInt timestamp
                commentIdBs = getEventId commentId
            in if BS.length rootIdBs == 32 && BS.length parentIdBs == 32 && BS.length commentIdBs == 32
               then BS.concat [rootIdBs, parentIdBs, timestampBs, commentIdBs]
               else error "Invalid event ID lengths in comment key")
        (\bs ->
            let len = BS.length bs
            in if len == 100  -- 32 (root) + 32 (parent) + 4 (timestamp) + 32 (comment)
               then let rootIdBs = BS.take 32 bs
                        parentIdBs = BS.take 32 (BS.drop 32 bs)
                        timestampBs = BS.take 4 (BS.drop 64 bs)
                        commentIdBs = BS.drop 68 bs
                    in if BS.length rootIdBs == 32 && BS.length parentIdBs == 32 && BS.length commentIdBs == 32
                       then Just ( EventId rootIdBs
                               , EventId parentIdBs
                               , decodeInt timestampBs
                               , EventId commentIdBs )
                       else Nothing
               else Nothing)
    )
    -- We can safely ignore the value since it's just `()`
    (Codec.unit)


-- | Encode an Int to a 4-byte ByteString
encodeInt :: Int -> [Word8]
encodeInt n = [fromIntegral (n `shiftR` 24),
               fromIntegral (n `shiftR` 16),
               fromIntegral (n `shiftR` 8),
               fromIntegral n]


-- | Decode a 4-byte ByteString to an Int
decodeInt :: BS.ByteString -> Int
decodeInt bs =
    case BS.unpack bs of
        [b1, b2, b3, b4] -> (fromIntegral b1 `shiftL` 24) .|.
                            (fromIntegral b2 `shiftL` 16) .|.
                            (fromIntegral b3 `shiftL` 8) .|.
                            fromIntegral b4
        _ -> 0  -- Default to 0 if we don't have exactly 4 bytes


-- | Initialize LMDB state
initializeLmdbState :: FilePath -> IO LmdbState
initializeLmdbState dbPath = do
    (env, lock) <- initializeEnv dbPath
    withTransaction env $ \txn -> do
        eventDb' <- openDatabase txn (Just "events") eventDbSettings
        eventRelaysDb' <- openDatabase txn (Just "event_relays") eventRelaysDbSettings
        followsDb' <- openDatabase txn (Just "follows") followsDbSettings
        profileDb' <- openDatabase txn (Just "profiles") defaultJsonSettings
        postTimelineDb' <- openDatabase txn (Just "post_timeline") defaultJsonSettings
        chatTimelineDb' <- openDatabase txn (Just "chat_timeline") defaultJsonSettings
        generalRelaysDb' <- openDatabase txn (Just "general_relays") defaultJsonSettings
        dmRelaysDb' <- openDatabase txn (Just "dm_relays") defaultJsonSettings
        -- latest_timestamps DB removed
        commentDb' <- openDatabase txn (Just "comments") commentDbSettings
        relayStatsDb' <- openDatabase txn (Just "relay_stats") defaultJsonSettings
        feedAnchorsDb' <- openDatabase txn (Just "feed_anchors") defaultJsonSettings

        pure $ LmdbState
            { lmdbLock = lock
            , lmdbEnv = env
            , eventDb = eventDb'
            , eventRelaysDb = eventRelaysDb'
            , profileDb = profileDb'
            , postTimelineDb = postTimelineDb'
            , chatTimelineDb = chatTimelineDb'
            , followsDb = followsDb'
            , generalRelaysDb = generalRelaysDb'
            , dmRelaysDb = dmRelaysDb'
            -- latestTimestampDb removed
            , commentDb = commentDb'
            , relayStatsDb = relayStatsDb'
            , feedAnchorsDb = feedAnchorsDb'
            , eventCache = LRU.newLRU (Just cacheSize)
            , eventRelaysCache = LRU.newLRU (Just cacheSize)
            , profileCache = LRU.newLRU (Just smallCacheSize)
            , followsCache = LRU.newLRU (Just smallCacheSize)
            , timelineCache = LRU.newLRU (Just cacheSize)
            , generalRelaysCache = LRU.newLRU (Just smallCacheSize)
            , dmRelaysCache = LRU.newLRU (Just smallCacheSize)
            , latestTimestampCache = LRU.newLRU (Just smallCacheSize)
            , commentCache = LRU.newLRU (Just cacheSize)
            , feedAnchorsCache = LRU.newLRU (Just smallCacheSize)
            }


-- | Cache size for general use
cacheSize :: Integer
cacheSize = 5000

-- | Small cache size for general use
smallCacheSize :: Integer
smallCacheSize = 500


-- | Mini cache size for comment subscriptions
miniCacheSize :: Integer
miniCacheSize = 50


-- | Initial LMDB state before login
initialLmdbState :: LmdbState
initialLmdbState = LmdbState
    { lmdbLock = error "LMDB not initialized"
    , lmdbEnv = error "LMDB not initialized"
    , eventDb = error "LMDB not initialized"
    , eventRelaysDb = error "LMDB not initialized"
    , profileDb = error "LMDB not initialized"
    , postTimelineDb = error "LMDB not initialized"
    , chatTimelineDb = error "LMDB not initialized"
    , followsDb = error "LMDB not initialized"
    , generalRelaysDb = error "LMDB not initialized"
    , dmRelaysDb = error "LMDB not initialized"
    -- latestTimestampDb removed
    , commentDb = error "LMDB not initialized"
    , relayStatsDb = error "LMDB not initialized"
    , feedAnchorsDb = error "LMDB not initialized"
    , eventCache = LRU.newLRU (Just cacheSize)
    , eventRelaysCache = LRU.newLRU (Just cacheSize)
    , profileCache = LRU.newLRU (Just smallCacheSize)
    , followsCache = LRU.newLRU (Just smallCacheSize)
    , timelineCache = LRU.newLRU (Just cacheSize)
    , generalRelaysCache = LRU.newLRU (Just smallCacheSize)
    , dmRelaysCache = LRU.newLRU (Just smallCacheSize)
    , latestTimestampCache = LRU.newLRU (Just smallCacheSize)
    , feedAnchorsCache = LRU.newLRU (Just smallCacheSize)
    , commentCache = LRU.newLRU (Just miniCacheSize)
    }

-- | Remove timeline entries for a given author
removeAuthorTimelineEntries :: TimelineType -> PubKeyXO -> LRU.LRU (TimelineType, PubKeyXO, Int) [EventId] -> LRU.LRU (TimelineType, PubKeyXO, Int) [EventId]
removeAuthorTimelineEntries timelineType author cache =
    let entries = LRU.toList cache
        newEntries = [(k, v) | (k@(tt, pk, _), v) <- entries, tt /= timelineType || pk /= author]
    in case LRU.maxSize cache of
        Just maxSize -> LRU.fromList (Just maxSize) newEntries
        Nothing -> LRU.fromList Nothing newEntries

-- | Helper to get an event directly from the database without using cache
getEventDirectFromDb :: (IOE :> es, State LmdbState :> es) => EventId -> Eff es (Maybe Event)
getEventDirectFromDb eid = do
    st <- get @LmdbState
    liftIO $ withTransaction (lmdbEnv st) $ \txn ->
        LMap.lookup' (readonly txn) (eventDb st) eid


-- | Query comments for a root ID
queryComments :: Transaction e -> Database CommentKey () -> EventId -> IO [(CommentKey, ())]
queryComments txn db rootId = do
    withCursor txn db $ \cursor -> do
        let startKey = (rootId, rootId, minBound, EventId $ BS.replicate 32 0)
        -- Get all comments that have this as their root, starting from newest
        allComments <- Pipes.toListM $
            LMap.lookupGteForward cursor startKey
            >-> Pipes.takeWhile (\kv -> case keyValueKey kv of (r, _, _, _) -> r == rootId)
            >-> Pipes.map (\kv -> (keyValueKey kv, keyValueValue kv))

        pure allComments

-- | Get comments from cache or database
getComments :: (IOE :> es, State LmdbState :> es) => EventId -> Eff es [CommentTree]
getComments rootId = do
    st <- get @LmdbState
    case LRU.lookup rootId (commentCache st) of
        (_, Just comments) -> pure comments
        (_, Nothing) -> do
            comments <- liftIO $ withTransaction (lmdbEnv st) $ \txn ->
                queryComments txn (commentDb st) rootId

            -- Build parent-child map directly from comment keys, maintaining order
            let parentMap = Map.fromListWith (flip (++)) $ map (\((_, parentId, _, commentId), _) ->
                    (parentId, [commentId])) comments
                rootChildren = fromMaybe [] $ Map.lookup rootId parentMap
                commentTree = map (\childId -> CommentTree childId (buildCommentTree childId parentMap)) rootChildren

            modify @LmdbState $ \s -> s { commentCache = LRU.insert rootId commentTree (commentCache s) }
            pure commentTree

-- | Build comment tree from parent-child map
buildCommentTree :: EventId -> Map EventId [EventId] -> [CommentTree]
buildCommentTree parentId parentMap =
    case Map.lookup parentId parentMap of
        Just children -> map (\childId -> CommentTree childId (buildCommentTree childId parentMap)) children
        Nothing -> []

-- | Flatten comments into a list of (eventId, indentation level) pairs
flattenComments :: [CommentTree] -> Int -> [(EventId, Int)]
flattenComments comments level =
    concatMap (\comment ->
        (commentId comment, level) :
        flattenComments (replies comment) (level + 1)
    ) comments
