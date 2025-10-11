module Presentation.Classes where

import Control.Monad (void)
import Data.Aeson (toJSON)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), defConfig, keyOrder)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, gets)
import Graphics.QML hiding (fireSignal)

import Downloader (Downloader, DownloadStatus(..), hasDownload, download)
import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (Event(..), EventId(..), Rumor(..), eventIdFromHex)
import Nostr.Event qualified as NE
import Nostr.Keys (PubKeyXO, exportPubKeyXO, keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.Relay (RelayPool(..))
import Nostr.Util (Util, formatDateTime, getKeyPair)
import QtQuick (QtQuick, QtQuickState(..), signalProfile)
import Store.Lmdb ( LmdbStore, getEvent, getEventRelays, getFollows)
import Types (AppState(..), Follow(..), Language(..), Post(..), PublishStatus(..))


-- Effect for creating C++ classes for usage in QtQuick QML
data Classes :: Effect where
    PostClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    ProfileClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)
    PublishStatusClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    FollowClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)
    CommentClass :: SignalKey (IO ()) -> FactoryPool EventId -> Classes m (Class (EventId, Int))


type instance DispatchOf Classes = Dynamic


postClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class EventId)
postClass changeKey = send $ PostClass changeKey

profileClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class PubKeyXO)
profileClass changeKey = send $ ProfileClass changeKey

publishStatusClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class EventId)
publishStatusClass changeKey = send $ PublishStatusClass changeKey

followClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class PubKeyXO)
followClass changeKey = send $ FollowClass changeKey

commentClass :: Classes :> es => SignalKey (IO ()) -> FactoryPool EventId -> Eff es (Class (EventId, Int))
commentClass changeKey postPool = send $ CommentClass changeKey postPool


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
    -- FeedClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    --     newClass [
    --         defPropertySigRO' "events" changeKey' $ \obj -> runE $ do
    --             modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { mainFeedEventsObjRef = Just obj } }
    --             cf <- gets @AppState currentFeed
    --             case cf of
    --                 Just (PostsFilter pk) -> do
    --                     eventIds <- getTimelineIds PostTimeline pk 100
    --                     events <- getEvents eventIds
    --                     posts <- mapM createPost events
    --                     mapM flattenPost posts
    --                 Just (PrivateMessagesFilter pk) -> do
    --                     eventIds <- getTimelineIds ChatTimeline pk 100
    --                     events <- getEvents eventIds
    --                     posts <- mapM createPost events
    --                     mapM flattenPost posts
    --                 Just (CommentsFilter _) -> error "CommentsFilter not supported for main feed"
    --                 Nothing -> error "No feed filter"
    --         ]
    --     where
    --         flattenPost :: ClassesEff es => Post -> Eff es [Text]
    --         flattenPost post = do
    --             relaysSet <- getEventRelays (postId post)
    --             let prettyConfig = defConfig {
    --                 confCompare = keyOrder [ "id", "pubkey", "created_at", "kind", "tags", "content", "sig"]
    --                     `mappend` compare
    --                 }
    --             return [
    --                 TE.decodeUtf8 $ B16.encode $ getEventId $ postId post,  -- id
    --                 eventToNevent (postEvent post) [],                      -- nevent
    --                 TE.decodeUtf8 $ BSL.toStrict $ encodePretty' prettyConfig $ toJSON $ postEvent post, -- raw
    --                 Text.intercalate "," $ Set.toList relaysSet,           -- relays
    --                 postType post,                                          -- postType
    --                 postContent post,                                       -- content
    --                 postTimestamp post,                                     -- timestamp
    --                 pubKeyXOToBech32 $ postAuthor post,                    -- authorId
    --                 maybe "" eventIdToNote (referencedPostId post) -- referencedPostId
    --                 ]

    PostClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "id" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              return $ TE.decodeUtf8 $ B16.encode $ getEventId eid,

            defPropertySigRO' "nevent" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              return $ eventToNevent (postEvent post) (Set.toList $ postRelays post),

            defPropertySigRO' "raw" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              let e = postEvent post
                  prettyConfig = defConfig {
                    confCompare = keyOrder [ "id", "pubkey", "created_at", "kind", "tags", "content", "sig"]
                      `mappend` compare
                  }
                  prettyEncode x = TE.decodeUtf8 . BSL.toStrict $ encodePretty' prettyConfig (toJSON x)
              case kind e of
                    NE.GiftWrap -> do
                      kp <- getKeyPair
                      sealed <- unwrapGiftWrap e kp
                      rumor <- maybe (return Nothing) (\s -> unwrapSeal s kp) sealed
                      return $ [prettyEncode e]
                           ++ maybe [] ((:[]) . prettyEncode) sealed
                           ++ maybe [] ((:[]) . prettyEncode) rumor
                    _ -> return [prettyEncode e],

            defPropertySigRO' "relays" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              return $ Set.toList $ postRelays post,

            defPropertySigRO' "postType" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              return $ postType post,

            defPropertySigRO' "content" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              return $ postContent post,

            defPropertySigRO' "timestamp" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              return $ postTimestamp post,

            defPropertySigRO' "authorId" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              return $ pubKeyXOToBech32 $ postAuthor post,

            defPropertySigRO' "referencedPostId" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              post <- loadPost eid
              return $ maybe "" eventIdToNote (referencedPostId post)
            ]

    ProfileClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "id" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                let value = TE.decodeUtf8 $ B16.encode $ exportPubKeyXO pk
                return value,

            defPropertySigRO' "npub" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                return $ pubKeyXOToBech32 pk,

            defPropertySigRO' "name" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                pure $ name <$> Map.lookup pk profileCache',

            defPropertySigRO' "displayName" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                pure $ displayName <$> Map.lookup pk profileCache',

            defPropertySigRO' "about" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                pure $ about <$> Map.lookup pk profileCache',

            defPropertySigRO' "picture" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                case Map.lookup pk profileCache' of
                    Just profile -> do
                      let url' = case picture profile of
                            Just url | Text.isPrefixOf "http://" url || Text.isPrefixOf "https://" url -> url
                            _ -> "https://robohash.org/" <> pubKeyXOToBech32 pk <> ".png?size=50x50"
                      check <- hasDownload url'
                      case check of
                        Ready (cacheFile, _mime, _expiry) -> pure $ Just $ "file://" <> pack cacheFile
                        Failed _ -> pure Nothing
                        Downloading -> pure Nothing
                        NotStarted -> do
                          void $ async $ do
                            void $ download url'
                            signalProfile obj
                          pure Nothing
                    Nothing -> pure Nothing,

            defPropertySigRO' "nip05" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                pure $ nip05 <$> Map.lookup pk profileCache',

            defPropertySigRO' "banner" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                case Map.lookup pk profileCache' of
                    Just profile -> do
                      let url' = case banner profile of
                            Just url | Text.isPrefixOf "http://" url || Text.isPrefixOf "https://" url -> Just url
                            _ -> Nothing
                      case url' of
                        Just url -> do
                          check <- hasDownload url
                          case check of
                            Ready (cacheFile, _mime, _expiry) -> pure $ "file://" <> pack cacheFile
                            Failed _ -> pure $ ""
                            Downloading -> pure $ ""
                            NotStarted -> do
                              void $ async $ do
                                void $ download url
                                signalProfile obj
                              pure $ ""
                        Nothing -> pure $ ""
                    Nothing -> pure $ "",

            defPropertySigRO' "isFollow" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                kp <- getKeyPair
                let currentPubKey = keyPairToPubKeyXO kp
                follows <- getFollows currentPubKey
                pure $ pk `elem` map pubkey follows,

            defPropertySigRO' "followerCount" changeKey' $ \_ -> do
                return (0 :: Int),
                {-
                st <- runE $ get @AppState
                let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
                runE $ getProfileEventCount subscribeToFollowers pk,
                -}

            defPropertySigRO' "followingCount" changeKey' $ \_ -> do
                return (0 :: Int)
                {-
                st <- runE $ get @AppState
                let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
                runE $ getProfileEventCount subscribeToFollowing pk
                -}
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

    FollowClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "pubkey" changeKey' $ \obj -> do
                let pubKeyXO = fromObjRef obj :: PubKeyXO
                return $ pubKeyXOToBech32 pubKeyXO,

            defPropertySigRO' "petname" changeKey' $ \obj -> do
                let pubKeyXO = fromObjRef obj :: PubKeyXO
                currentFollows <- runE $ gets @AppState currentFollows
                return $ fromMaybe "" $ maybe Nothing petName $ Map.lookup pubKeyXO currentFollows,

            defPropertySigRO' "displayName" changeKey' $ \obj -> runE $ do
                let pubKeyXO = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                pure $ fromMaybe "" $ displayName =<< Map.lookup pubKeyXO profileCache',

            defPropertySigRO' "name" changeKey' $ \obj -> runE $ do
                let pubKeyXO = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                pure $ fromMaybe "" $ name =<< Map.lookup pubKeyXO profileCache',

            defPropertySigRO' "picture" changeKey' $ \obj -> runE $ do
                let pubKeyXO = fromObjRef obj :: PubKeyXO
                profileCache' <- gets @AppState profileCache
                case Map.lookup pubKeyXO profileCache' of
                    Just profile -> do
                      let url' = case picture profile of
                            Just url | Text.isPrefixOf "http://" url || Text.isPrefixOf "https://" url -> url
                            _ -> "https://robohash.org/" <> pubKeyXOToBech32 pubKeyXO <> ".png?size=50x50"
                      check <- hasDownload url'
                      case check of
                        Ready (cacheFile, _mime, _expiry) -> pure $ Just $ "file://" <> pack cacheFile
                        Failed _ -> pure Nothing
                        Downloading -> pure Nothing
                        NotStarted -> do
                          void $ async $ do
                            void $ download url'
                            signalProfile obj
                          pure Nothing
                    Nothing -> pure Nothing,

            defPropertySigRO' "follow_type" changeKey' $ \obj -> do
                let pubKeyXO = fromObjRef obj :: PubKeyXO
                currentFollows <- runE $ gets @AppState currentFollows
                return $ if Map.member pubKeyXO currentFollows then ("follow" :: Text) else ("all" :: Text)
          ]

    CommentClass changeKey' postPool -> liftIO $ do
      newClass [
        defPropertySigRO' "post" changeKey' $ \obj -> do
            let (eid, _) = fromObjRef obj :: (EventId, Int)
            liftIO $ getPoolObject postPool eid,

        defPropertySigRO' "indentationLevel" changeKey' $ \obj -> do
            let (_, level) = fromObjRef obj :: (EventId, Int)
            return level
        ]


-- | Create a post from an event.
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
  relays <- getEventRelays (eventId ev)
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
    , postRelays = relays
    , referencedPostId = case kind ev of
        NE.Repost -> listToMaybe [ fromMaybe (error "Invalid event ID") $ eventIdFromHex eidStr | ("e":eidStr:_) <- tags ev ]
        _ -> Nothing
    }

loadPost :: ClassesEff es => EventId -> Eff es Post
loadPost eid = do
    cache <- gets @AppState currentFeedCache
    case Map.lookup eid cache of
        Just post -> return post
        Nothing -> do
          ev <- getEvent eid
          case ev of
            Just ev' -> do
              post <- createPost ev'
              return post
            Nothing -> do
              logDebug $ "Post not found in cache: " <> pack (show eid)
              error "Post not found in cache"
