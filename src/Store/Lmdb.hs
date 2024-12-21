{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Store.Lmdb where

import Control.Monad (forM_,void)
import Data.Aeson (ToJSON, FromJSON, encode, decode, eitherDecode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
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
import Lmdb.Types
import Pipes.Prelude qualified as Pipes
import System.FilePath ((</>))
import Pipes ((>->))

import Logging
import Nostr.Event (validateEvent, unwrapGiftWrap, unwrapSeal)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Types ( Event(..), EventId(..), Kind(..), Profile, Relay(..), Tag(..)
                   , Rumor(..), rumorPubKey, rumorTags, rumorCreatedAt, emptyProfile )
import Types (AppState(..), EventWithRelays(..), Follow(..))


-- | Timeline types
data TimelineType = PostTimeline | ChatTimeline
    deriving (Show, Eq)

-- | Timeline key type
type TimelineKey = (PubKeyXO, Int)


-- | LmdbStore operations
data LmdbStore :: Effect where
    -- Event operations (now handles all storage operations)
    PutEvent :: EventWithRelays -> LmdbStore m ()
    PutGiftWrap :: EventWithRelays -> [PubKeyXO] -> Int -> LmdbStore m ()
    
    -- Query operations (read-only)
    GetEvent :: EventId -> LmdbStore m (Maybe EventWithRelays)
    GetFollows :: PubKeyXO -> LmdbStore m [Follow]
    GetProfile :: PubKeyXO -> LmdbStore m (Profile, Int)
    GetTimelineIds :: TimelineType -> PubKeyXO -> Int -> LmdbStore m [EventId]


type instance DispatchOf LmdbStore = Dynamic

makeEffect ''LmdbStore


-- | Run LmdbEffect
runLmdbStore :: (IOE :> es, State AppState :> es, Logging :> es)
             => Eff (LmdbStore : es) a
             -> Eff es a
runLmdbStore = interpret $ \env -> \case
    -- Event operations (main storage operation)
    PutEvent ev -> do
        logDebug $ "Putting event: " <> pack (show ev)
        st <- get
        case (eventDb st, followsDb st, profileDb st, postTimelineDb st, chatTimelineDb st, lmdbEnv st, keyPair st) of
            (Just eventDb', Just followsDb', Just profileDb', Just postTimelineDb', Just chatTimelineDb', Just env', Just kp) -> 
                liftIO $ withTransaction env' $ \txn -> do
                    putStrLn "Starting LMDB transaction..."
                    Map.repsert' txn eventDb' (eventId $ event ev) ev
                    putStrLn "Inserted into event database"

                    case kind (event ev) of
                        GiftWrap -> do
                            mSealedEvent <- liftIO $ unwrapGiftWrap (event ev) kp
                            case mSealedEvent of
                                Just sealedEvent | validateEvent sealedEvent -> 
                                    case kind sealedEvent of
                                        Seal -> do
                                            mDecryptedRumor <- liftIO $ unwrapSeal sealedEvent kp
                                            case mDecryptedRumor of
                                                Just decryptedRumor | pubKey sealedEvent == rumorPubKey decryptedRumor -> do
                                                    let participants = if rumorPubKey decryptedRumor == keyPairToPubKeyXO kp
                                                          then sort $ getAllPTags (rumorTags decryptedRumor)
                                                          else filter (/= keyPairToPubKeyXO kp) $ rumorPubKey decryptedRumor : sort (getAllPTags (rumorTags decryptedRumor))
                                                    addTimelineEntryTx txn chatTimelineDb' ev participants (rumorCreatedAt decryptedRumor)
                                                _ -> pure ()
                                        _ -> pure ()
                                _ -> pure ()

                        ShortTextNote -> do
                            addTimelineEntryTx txn postTimelineDb' ev [pubKey $ event ev] (createdAt $ event ev)

                        Repost -> case ([t | t@(ETag _ _ _) <- tags (event ev)], eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev)) of
                            (ETag _ _ _:_, Right originalEvent) | validateEvent originalEvent -> do
                                addTimelineEntryTx txn postTimelineDb' ev [pubKey $ event ev] (createdAt $ event ev)
                            _ -> pure ()

                        EventDeletion -> do
                            let eventIdsToDelete = [eid | ETag eid _ _ <- tags (event ev)]
                            forM_ eventIdsToDelete $ \eid -> do
                                mEvent <- Map.lookup' (readonly txn) eventDb' eid
                                case mEvent of
                                    Just deletedEv -> do
                                        let key = (pubKey $ event deletedEv, createdAt $ event deletedEv)
                                            db = case kind (event deletedEv) of
                                                ShortTextNote -> postTimelineDb'
                                                Repost -> postTimelineDb'
                                                _ -> chatTimelineDb'
                                        Map.delete' txn db key
                                        Map.delete' txn eventDb' eid
                                    Nothing -> pure ()

                        Metadata -> 
                            case eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev) of
                                Right profile -> 
                                    Map.repsert' txn profileDb' (pubKey $ event ev) (profile, createdAt $ event ev)
                                Left _ -> pure ()

                        FollowList -> do
                            putStrLn "Processing FollowList..."
                            let followList' = [Follow pk (fmap InboxRelay relay') petName' | PTag pk relay' petName' <- tags (event ev)]
                            putStrLn $ "Created follow list: " ++ show followList'
                            
                            putStrLn "About to insert into follows database..."
                            Map.repsert' txn followsDb' (pubKey $ event ev) followList'
                            putStrLn "Inserted into follows database"

                        _ -> pure ()

            _ -> throwIO $ userError "Required databases not initialized"

    GetEvent eid -> do
        logDebug $ "Getting event: " <> pack (show eid)
        st <- get
        case (eventDb st, lmdbEnv st) of
            (Just eventDb', Just env') -> liftIO $ withTransaction env' $ \txn ->
                Map.lookup' (readonly txn) eventDb' eid
            _ -> throwIO $ userError "Event database not initialized"

    -- Query operations (read-only)
    GetFollows pk -> do
        logDebug $ "Getting follows: " <> pack (show pk)
        st <- get
        case (followsDb st, lmdbEnv st) of
            (Just followsDb', Just env') -> liftIO $ withTransaction env' $ \txn -> do
                mFollows <- Map.lookup' (readonly txn) followsDb' pk
                pure $ maybe [] id mFollows
            _ -> throwIO $ userError "Follows database not initialized"

    GetProfile pk -> do
        logDebug $ "Getting profile: " <> pack (show pk)
        st <- get
        case (lmdbEnv st, profileDb st) of
            (Just env', Just profileDb') -> liftIO $ withTransaction env' $ \txn -> do
                mProfile <- Map.lookup' (readonly txn) profileDb' pk
                pure $ maybe (emptyProfile, 0) id mProfile
            _ -> throwIO $ userError "Profile database not initialized"

    GetTimelineIds timelineType author limit -> do
        logDebug $ "Getting timeline ids: " <> pack (show timelineType) <> " " <> pack (show author) <> " " <> pack (show limit)
        st <- get
        let dbSelector = case timelineType of
                PostTimeline -> postTimelineDb
                ChatTimeline -> chatTimelineDb
        case (lmdbEnv st, dbSelector st) of
            (Just env', Just timelineDb') -> liftIO $ withTransaction env' $ \txn ->
                withCursor (readonly txn) timelineDb' $ \cursor ->
                    Pipes.toListM $
                        Map.lastBackward cursor
                        >-> Pipes.filter (\kv -> fst (keyValueKey kv) == author)
                        >-> Pipes.map keyValueValue
                        >-> Pipes.take limit
            _ -> throwIO $ userError "Timeline database not initialized"

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


-- | Default Lmdb settings for JSON-serializable types
defaultJsonSettings :: (Ord k, ToJSON k, FromJSON k, ToJSON v, FromJSON v) 
                   => DatabaseSettings k v
defaultJsonSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (toStrict . encode)
        (decode . fromStrict))
    (Codec.throughByteString
        (toStrict . encode)
        (decode . fromStrict))


-- Lmdb configuration
maxMapSize :: Int
maxMapSize = 500_000_000_000 -- 500 GB

maxReaders :: Int
maxReaders = 120

maxDbs :: Int
maxDbs = 8 -- currently 5 are required, leave some room for future growth

-- | Initialize the Lmdb environment
initializeEnv :: FilePath -> IO (Environment ReadWrite)
initializeEnv dbPath = 
    initializeReadWriteEnvironment maxMapSize maxReaders maxDbs dbPath

-- | Initialize the event database
initEventDb :: Transaction 'ReadWrite -> IO (Database EventId EventWithRelays)
initEventDb txn = openDatabase txn (Just "events") eventDbSettings

-- | Initialize the follows database
initFollowsDb :: Transaction 'ReadWrite -> IO (Database PubKeyXO [Follow])
initFollowsDb txn = openDatabase txn (Just "follows") defaultJsonSettings

-- | Initialize the profile database
initProfileDb :: Transaction 'ReadWrite -> IO (Database PubKeyXO (Profile, Int))
initProfileDb txn = openDatabase txn (Just "profiles") defaultJsonSettings

-- | Initialize the post timeline database
initPostTimelineDb :: Transaction 'ReadWrite -> IO (Database TimelineKey EventId)
initPostTimelineDb txn = openDatabase txn (Just "post_timeline") defaultJsonSettings

-- | Initialize the chat timeline database
initChatTimelineDb :: Transaction 'ReadWrite -> IO (Database TimelineKey EventId)
initChatTimelineDb txn = openDatabase txn (Just "chat_timeline") defaultJsonSettings

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
            Nothing -> error "Failed to decode EventWithRelays"))

-- | Get all p tags from the rumor tags
getAllPTags :: [Tag] -> [PubKeyXO]
getAllPTags = mapMaybe extractPubKey
  where
    extractPubKey (PTag pk _ _) = Just pk
    extractPubKey _ = Nothing
