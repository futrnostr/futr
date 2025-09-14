module Presentation.Classes where

import Control.Monad (void)
import Data.Aeson (toJSON)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), defConfig, keyOrder)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem
import Effectful.State.Static.Shared (State, get, gets,modify)
import Graphics.QML
import System.FilePath ((</>), takeExtension, dropExtension)

import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (Event(..), EventId(..), Rumor(..), eventIdFromHex)
import Nostr.Event qualified as NE
import Nostr.Keys (PubKeyXO, exportPubKeyXO, keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.ProfileManager (ProfileManager)
import Nostr.Relay (RelayPool(..))
import Nostr.Util (Util, formatDateTime, getKeyPair)
import QtQuick ( QtQuick, PropertyMap(..), PropertyName, QtQuickState(..)
               , UIReferences(..), UIUpdates(..), emptyUpdates, notify )
import Store.Lmdb (LmdbStore, TimelineType(..), getCommentsWithIndentationLevel, getEvent, getEvents, getEventRelays, getFollows, getTimelineIds)
import Types (AppState(..), FeedFilter(..), Follow(..), Language(..), Post(..), PublishStatus(..))
import Downloader (Downloader(..), DownloadStatus(..), hasDownload, download, peekMimeType)


-- Effect for creating C++ classes for usage in QtQuick QML
data Classes :: Effect where
    FeedClass :: SignalKey (IO ()) -> Classes m (Class ())
    CommentFeedClass :: SignalKey (IO ()) -> Classes m (Class ())
    PublishStatusClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    CommentClass :: SignalKey (IO ()) -> Classes m (Class (EventId, Int))


type instance DispatchOf Classes = Dynamic


feedClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class ())
feedClass changeKey = send $ FeedClass changeKey

commentFeedClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class ())
commentFeedClass changeKey = send $ CommentFeedClass changeKey

publishStatusClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class EventId)
publishStatusClass changeKey = send $ PublishStatusClass changeKey

commentClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class (EventId, Int))
commentClass changeKey = send $ CommentClass changeKey


-- | SubscriptionEff
type ClassesEff es =
  ( QtQuick :> es
  , State QtQuickState :> es
  , State RelayPool :> es
  , State AppState :> es
  , LmdbStore :> es
  , Downloader :> es
  , Concurrent :> es
  , Nostr :> es
  , Logging :> es
  , Util :> es
  , IOE :> es
  )


-- | Handler for subscription effects.
runClasses :: ClassesEff es => Eff (Classes : es) a -> Eff es a
runClasses = interpret $ \_ -> \case
    FeedClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "events" changeKey' $ \obj -> runE $ do
                liftIO $ putStrLn "FeedClass events"
                modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { mainFeedEventsObjRef = Just obj } }
                cf <- gets @AppState currentFeed
                case cf of
                    Just (PostsFilter pk) -> do
                        eventIds <- getTimelineIds PostTimeline pk 100
                        events <- getEvents eventIds
                        posts <- mapM createPost events
                        mapM flattenPost posts
                    Just (PrivateMessagesFilter pk) -> do
                        eventIds <- getTimelineIds ChatTimeline pk 100
                        events <- getEvents eventIds
                        posts <- mapM createPost events
                        mapM flattenPost posts
                    Just (CommentsFilter _) -> error "CommentsFilter not supported for main feed"
                    Nothing -> error "No feed filter"
            ]
        where
            flattenPost :: ClassesEff es => Post -> Eff es [Text]
            flattenPost post = do
                relaysSet <- getEventRelays (postId post)
                let prettyConfig = defConfig {
                    confCompare = keyOrder [ "id", "pubkey", "created_at", "kind", "tags", "content", "sig"]
                        `mappend` compare
                    }
                return [
                    TE.decodeUtf8 $ B16.encode $ getEventId $ postId post,  -- id
                    eventToNevent (postEvent post) [],                      -- nevent
                    TE.decodeUtf8 $ BSL.toStrict $ encodePretty' prettyConfig $ toJSON $ postEvent post, -- raw
                    Text.intercalate "," $ Set.toList relaysSet,           -- relays
                    postType post,                                          -- postType
                    postContent post,                                       -- content
                    postTimestamp post,                                     -- timestamp
                    pubKeyXOToBech32 $ postAuthor post,                    -- authorId
                    maybe "" eventIdToNote (referencedPostId post) -- referencedPostId
                    ]

    PublishStatusClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "eventId" changeKey' $ \obj -> do
                let eid = fromObjRef obj :: EventId
                return $ TE.decodeUtf8 $ B16.encode $ getEventId eid,

            defPropertySigRO' "relayStatuses" changeKey' $ \obj -> do
                let eid = fromObjRef obj :: EventId
                st <- runE $ get @RelayPool
                return $ case Map.lookup eid (publishStatus st) of
                    Just statusMap ->
                        [ [relay, if status == Success then ("" :: Text) else
                            case status of
                                Failure msg -> msg
                                _ -> ""]
                        | (relay, status) <- Map.toList statusMap ]
                    Nothing -> []
            ]

    CommentClass changeKey' -> createCommentClass changeKey'

    CommentFeedClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "events" changeKey' $ \obj -> runE $ do
                modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { commentFeedEventsObjRef = Just obj } }
                cf <- gets @AppState currentCommentFeed
                case cf of
                    Just (CommentsFilter eid) -> do
                        commentIds <- getCommentsWithIndentationLevel eid
                        let commentEventIds = map fst commentIds
                        events <- getEvents commentEventIds
                        posts <- mapM createPost events
                        mapM flattenPost posts
                    _ -> error "Invalid feed filter"
            ]
        where
            flattenPost :: ClassesEff es => Post -> Eff es [Text]
            flattenPost post = do
                relaysSet <- getEventRelays (postId post)
                let prettyConfig = defConfig {
                    confCompare = keyOrder [ "id", "pubkey", "created_at", "kind", "tags", "content", "sig"]
                        `mappend` compare
                    }
                return [
                    TE.decodeUtf8 $ B16.encode $ getEventId $ postId post,  -- id
                    eventToNevent (postEvent post) [],                      -- nevent
                    TE.decodeUtf8 $ BSL.toStrict $ encodePretty' prettyConfig $ toJSON $ postEvent post, -- raw
                    Text.intercalate "," $ Set.toList relaysSet,           -- relays
                    postType post,                                          -- postType
                    postContent post,                                       -- content
                    postTimestamp post,                                     -- timestamp
                    pubKeyXOToBech32 $ postAuthor post,                    -- authorId
                    maybe "" eventIdToNote (referencedPostId post) -- referencedPostId
                    ]



createCommentClass :: ClassesEff es => SignalKey (IO ()) -> Eff es (Class (EventId, Int))
createCommentClass changeKey' = liftIO $ do
    newClass [
        defPropertySigRO' "post" changeKey' $ \obj -> do
            let (eid, _) = fromObjRef obj :: (EventId, Int)
            return $ TE.decodeUtf8 $ B16.encode $ getEventId eid,

        defPropertySigRO' "indentationLevel" changeKey' $ \obj -> do
            let (_, level) = fromObjRef obj :: (EventId, Int)
            return level
        ]


-- | Store a profile object reference in the property map
storeProfileObjRef :: ClassesEff es => PubKeyXO -> PropertyName -> ObjRef PubKeyXO -> Eff es ()
storeProfileObjRef pk propName obj = withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    weakObjRef <- toWeakObjRef obj

    runE $ modify @QtQuickState $ \st ->
        let currentMap = profileObjRefs (propertyMap st)
            updatedMap = Map.alter (Just . maybe 
                                         (Map.singleton propName weakObjRef)
                                         (Map.insert propName weakObjRef)) 
                                 pk 
                                 currentMap
        in st { propertyMap = (propertyMap st) { profileObjRefs = updatedMap } }

    finalizer <- newObjFinaliser $ \_ -> do
        runE $ modify @QtQuickState $ \st' ->
            let currentMap = profileObjRefs (propertyMap st')
                updatedMap = Map.delete pk currentMap
            in st' { propertyMap = (propertyMap st') { profileObjRefs = updatedMap } }

    addObjFinaliser finalizer obj


-- Helper function to find an available filename
findAvailableFilename :: (FileSystem :> es) => FilePath -> FilePath -> Eff es FilePath
findAvailableFilename dir baseFileName = do
  let tryPath = dir </> baseFileName
  exists <- doesFileExist tryPath
  if not exists
    then return tryPath
    else findNextAvailable dir baseFileName 1
  where
    findNextAvailable :: (FileSystem :> es) => FilePath -> FilePath -> Int -> Eff es FilePath
    findNextAvailable dir' fileName counter = do
      let ext = takeExtension fileName
          baseName = dropExtension fileName
          newName = baseName ++ " (" ++ show counter ++ ")" ++ ext
          tryPath = dir' </> newName
      exists <- doesFileExist tryPath
      if not exists
        then return tryPath
        else findNextAvailable dir' fileName (counter + 1)





-- | Create a post from an event.
-- @todo code duplication with Futr.hs
createPost :: ClassesEff es => Event -> Eff es Post
createPost ev = do
  (authorPubKey, content', ts) <- case kind ev of
      NE.GiftWrap -> do
        kp <- getKeyPair
        sealed <- unwrapGiftWrap ev kp
        rumor <- maybe (return Nothing) (unwrapSeal `flip` kp) sealed
        return $ (fromMaybe (pubKey ev) (rumorPubKey <$> rumor), fromMaybe "" $ rumorContent <$> rumor, fromMaybe 0 $ rumorCreatedAt <$> rumor)
      NE.Repost -> do
        let repostedId = listToMaybe [ fromMaybe (error "Invalid event ID") $ eventIdFromHex eidStr | ("e":eidStr:_) <- tags ev ]
        case repostedId of
            Just eid' -> do
              return $ (pubKey ev, "nostr:" <> eventIdToNote eid', createdAt ev)
            Nothing -> return (pubKey ev, "", createdAt ev)
      _ -> return (pubKey ev, content ev, createdAt ev)
  formattedTime <- formatDateTime English ts
  pure Post
    { postId = eventId ev
    , postAuthor = authorPubKey
    , postEvent = ev
    , postContent = content'
    , postTimestamp = formattedTime
    , postType = case kind ev of
        NE.ShortTextNote ->
          if any (\t -> case t of
                        ("q":_) -> True
                        _ -> False) (tags ev)
          then "quote_repost"
          else "short_text_note"
        NE.Repost -> "repost"
        NE.Comment -> "comment"
        NE.GiftWrap -> "gift_wrap"
        _ -> "unknown"
    , referencedPostId = case kind ev of
        NE.Repost -> listToMaybe [ fromMaybe (error "Invalid event ID") $ eventIdFromHex eidStr | ("e":eidStr:_) <- tags ev ]
        _ -> Nothing
    }
