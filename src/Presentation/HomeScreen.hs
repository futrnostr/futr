{-# LANGUAGE BlockArguments #-}

module Presentation.HomeScreen where

import Control.Lens ((^.))
import Control.Monad (filterM, forM_, void)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Exception (try, SomeException)
import Effectful.FileSystem
import Effectful.State.Static.Shared (get, gets, modify)
import QtQuick
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Graphics.QML qualified as QML
import Network.Wreq qualified as Wreq
import Prelude hiding (drop)
import System.FilePath ((</>), dropExtension, takeExtension, takeFileName)
import Text.Read (readMaybe)

import Downloader (Downloader, DownloadStatus(..), hasDownload, peekMimeType, download)
import KeyMgmt (AccountId(..), updateProfile)
import Nostr
import Nostr.Bech32 (pubKeyXOToBech32, bech32ToPubKeyXO, noteToEventId, neventToEvent, naddrToEvent)
import Nostr.Event (Event(..), EventId(..), createMetadata)
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.ProfileManager (getProfile)
import Nostr.Publisher
import Nostr.Relay (RelayPool(..))
import Nostr.Util
import Presentation.Classes qualified as Classes
import Presentation.Classes (createPost)
import Presentation.KeyMgmtUI qualified as KeyMgmtUI
import Presentation.RelayMgmtUI qualified as RelayMgmtUI
import Futr hiding (Comment, QuoteRepost, Repost)
import Store.Lmdb ( TimelineType(..), getCommentsWithIndentationLevel, getEvent
                  , getEvents, getTimelineIds )
import Types (Post(..), AppState(..), AppScreen(..), FeedFilter(..))


-- | HomeScren Effect for creating QML UI.
data HomeScreen :: Effect where
  CreateUI :: SignalKey (IO ()) -> HomeScreen m (ObjRef ())


-- | Dispatch type for UI effect.
type instance DispatchOf HomeScreen = Dynamic


createUI :: HomeScreen :> es => SignalKey (IO ()) -> Eff es (ObjRef ())
createUI changeKey = send $ CreateUI changeKey


-- | Run the UI effect.
runHomeScreen :: (FutrEff es, Futr :> es) => Eff (HomeScreen : es) a -> Eff es a
runHomeScreen = interpret $ \_ -> \case
  CreateUI changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ KeyMgmtUI.createUI changeKey'
    relayMgmtObj <- runE $ RelayMgmtUI.createUI changeKey'

    postClass' <- runE $ Classes.postClass changeKey'
    profileClass' <- runE $ Classes.profileClass changeKey'
    publishStatusClass' <- runE $ Classes.publishStatusClass changeKey'
    followClass' <- runE $ Classes.followClass changeKey'

    postPool <- newFactoryPool (newObject postClass')
    profilePool <- newFactoryPool (newObject profileClass')
    publishStatusPool <- newFactoryPool (newObject publishStatusClass')
    followPool <- newFactoryPool (newObject followClass')

    commentClass' <- runE $ Classes.commentClass changeKey' postPool
    commentPool <- newFactoryPool (newObject commentClass')

    rootClass <- newClass [
        defPropertyConst' "version" (\_ -> do
          st <- runE $ get @AppState
          return $ version st
        ),

        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertyConst' "ctxRelayMgmt" (\_ -> return relayMgmtObj),

        -- defPropertyConst' "currentProfile" (\_ -> do
        --   mp <- runE $ gets @AppState currentProfile
        --   case mp of
        --     Just (pk, _) -> do
        --       prof <- runE $ getProfile pk
        --       kpCur <- runE $ getKeyPair
        --       let currentPubKey = keyPairToPubKeyXO kpCur
        --       followsCur <- runE $ getFollows currentPubKey
        --       let isFollowBool = pk `elem` map pubkey followsCur
        --       return $ Just
        --         [ TE.decodeUtf8 $ B16.encode $ exportPubKeyXO pk  -- id (hex)
        --         , pubKeyXOToBech32 pk                             -- npub
        --         , fromMaybe "" (name prof)
        --         , fromMaybe "" (displayName prof)
        --         , fromMaybe "" (about prof)
        --         , fromMaybe "" (picture prof)  -- return raw URL, let QML handle resolution
        --         , fromMaybe "" (NP.nip05 prof)
        --         , fromMaybe "" (banner prof)   -- return raw URL, let QML handle resolution
        --         , if isFollowBool then "1" else "0"
        --         ]
        --     Nothing -> return (Nothing :: Maybe [Text])),

        defPropertySigRW' "currentScreen" changeKey'
          (\_ -> do
            st <- runE $ get @AppState
            return $ pack $ show $ currentScreen st)
          (\obj newScreen -> do
              case readMaybe (unpack newScreen) :: Maybe AppScreen of
                  Just s -> do
                      runE $ do
                        modify $ \st -> st { currentScreen = s }
                        fireSignal obj
                  Nothing -> return ()),

        defPropertySigRO' "mynpub" changeKey' $ \_ -> runE $
          pubKeyXOToBech32 . keyPairToPubKeyXO <$> getKeyPair,

        defPropertySigRO' "mypicture" changeKey' $ \obj -> runE $ do
          xo <- keyPairToPubKeyXO <$> getKeyPair
          profileCache' <- gets @AppState profileCache
          case Map.lookup xo profileCache' of
              Just profile -> do
                let url' = case picture profile of
                      Just url | Text.isPrefixOf "http://" url || Text.isPrefixOf "https://" url -> url
                      _ -> "https://robohash.org/" <> pubKeyXOToBech32 xo <> ".png?size=50x50"
                check <- hasDownload url'
                case check of
                  Ready (cacheFile, _mime, _expiry) -> pure $ Just $ "file://" <> pack cacheFile
                  Failed _ -> pure Nothing
                  Downloading -> pure Nothing
                  NotStarted -> do
                    void $ async $ do
                      void $ download url'
                      fireSignal obj
                    pure Nothing
              Nothing -> pure Nothing,

        defPropertySigRO' "inboxModelState" changeKey' $ \obj -> runE $ do
          modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { inboxModelStateObjRef = Just obj } }
          st <- get @AppState
          return $ pack $ show $ inboxModelState st,

        defSignal "loginStatusChanged" (Proxy :: Proxy LoginStatusChanged),

        defMethod' "login" $ \obj input -> runE $ login obj input,

        defMethod' "logout" $ \obj -> runE $ logout obj,

        defMethod' "search" $ \obj input -> runE $ do
          res <- search obj input
          return $ TE.decodeUtf8 $ BSL.toStrict $ encode res,

        defMethod' "setFeed" $ \(_ :: ObjRef ()) (feedType :: Text) (input :: Text) -> runE $ do
          --logDebug $ "setFeed: " <> feedType <> " " <> input
          let f = case feedType of
                "public" -> case bech32ToPubKeyXO input of
                  Just pubKeyXO -> PostsFilter pubKeyXO
                  Nothing -> error "Invalid bech32 public key"
                "private" -> case bech32ToPubKeyXO input of
                  Just pubKeyXO -> PrivateMessagesFilter pubKeyXO
                  Nothing -> error "Invalid bech32 public key"
                _ -> error "Invalid feed type"
          
          loadFeed f
          uiRefs' <- gets @QtQuickState uiRefs
          forM_ (mainFeedEventsObjRef uiRefs') $ \obj -> fireSignal obj
          return (),

        -- defMethod' "loadFeed" $ \(_ :: ObjRef ()) (feedType :: Text) (input :: Text) -> runE $ do
        --   logDebug $ "loadFeed: " <> feedType <> " " <> input
        --   let f = case feedType of
        --         "public" -> case bech32ToPubKeyXO input of
        --           Just pubKeyXO -> PostsFilter pubKeyXO
        --           Nothing -> error "Invalid bech32 public key"
        --         "private" -> case bech32ToPubKeyXO input of
        --           Just pubKeyXO -> PrivateMessagesFilter pubKeyXO
        --           Nothing -> error "Invalid bech32 public key"
        --         _ -> error "Invalid feed type"
        --   loadFeed f
        --   ck <- gets @QtQuickState signalKey
        --   let ck' = fromMaybe (error "Signal key not found") ck
        --   feedClass' <- Classes.feedClass ck'
        --   feedObj <- liftIO $ newObject feedClass' ()
        --   return (feedObj :: ObjRef ()),

        defMethod' "saveProfile" $ \_ input -> runE $ do
          let profile = maybe (error "Invalid profile JSON") id $ decode (BSL.fromStrict $ TE.encodeUtf8 input) :: Profile
          n <- getCurrentTime
          kp <- getKeyPair
          let unsigned = createMetadata profile (keyPairToPubKeyXO kp) n
          signedMaybe <- signEvent unsigned kp
          case signedMaybe of
            Nothing -> error "Failed to sign profile update event"
            Just signed -> do
              broadcast signed
              let aid = AccountId $ pubKeyXOToBech32 (keyPairToPubKeyXO kp)
              updateProfile aid profile,

        defPropertySigRO' "followList" changeKey' $ \obj -> do
          runE $ modify $ \s -> s { uiRefs = (uiRefs s) { followsObjRef = Just obj } }
          userPubKey <- runE $ keyPairToPubKeyXO <$> getKeyPair
          myFollows <- runE $ gets @AppState currentFollows
          -- Include user's own profile first, then follows
          userFollowObj <- getPoolObject followPool userPubKey
          runE $ mapM_ getProfile $ Map.keys myFollows
          followObjects <- mapM (getPoolObject followPool) $ Map.keys myFollows
          return $ userFollowObj : followObjects,
          -- let petnameOf pk = maybe "" (fromMaybe "" . petName) (listToMaybe [ f | f <- follows, pubkey f == pk ])
          -- followData <- forM (map pubkey follows) $ \pk -> do
          --   profile <- getProfile pk
          --   return
          --     [ pubKeyXOToBech32 pk
          --     , petnameOf pk
          --     , fromMaybe "" (displayName profile)
          --     , fromMaybe "" (name profile)
          --     , fromMaybe "" (picture profile)  -- return raw URL, let QML handle resolution
          --     , "follow"
          --     ]
          -- return $
          --   -- what's need feed first
          --   [["", "", "What's new", "", "qrc:/icons/news.svg", "feed"]] ++
          --   -- user profile second
          --   [[pubKeyXOToBech32 userPubKey, "", "", "", fromMaybe "" (picture userProfile), "feed"]] ++
          --   -- follows third
          --   followData,

        defPropertySigRO' "currentFeed" changeKey' $ \obj -> runE $ do
          modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { mainFeedEventsObjRef = Just obj } }
          cf <- gets @AppState currentFeed
          es <- case cf of
              Just (PostsFilter pk) -> do
                  eventIds <- getTimelineIds PostTimeline pk 100
                  getEvents eventIds
              Just (PrivateMessagesFilter pk) -> do
                  eventIds <- getTimelineIds ChatTimeline pk 100
                  getEvents eventIds
              Just (CommentsFilter _) -> error "CommentsFilter not supported for main feed"
              Nothing -> error "No feed filter"
          posts <- mapM createPost es
          modify @AppState $ \st -> st { currentFeedCache = Map.fromList $ zip (map postId posts) posts }
          liftIO $ mapM (getPoolObject postPool) (map eventId es),

        defPropertySigRO' "commentFeed" changeKey' $ \obj -> runE $ do
          modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { commentFeedEventsObjRef = Just obj } }
          cf <- gets @AppState currentCommentFeed
          case cf of
              Just (CommentsFilter eid') -> do
                  comments <- getCommentsWithIndentationLevel eid'
                  liftIO $ mapM (getPoolObject commentPool) comments
              _ -> error "No comment feed filter",

        defPropertySigRO' "publishStatuses" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { publishStatusObjRef = Just obj } }
          st <- runE $ get @RelayPool
          now <- runE getCurrentTime
          -- Filter statuses where event is newer than 10 seconds
          let statusMap = publishStatus st
          recentEids <- filterM (\eid -> do
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                  Just event ->
                      return $ createdAt event  + 10 > now
                  Nothing -> return False) (Map.keys statusMap)
          mapM (getPoolObject publishStatusPool) recentEids,

        defMethod' "follow" $ \_ npubText -> runE $ followProfile npubText,

        defMethod' "unfollow" $ \_ npubText -> runE $ unfollowProfile npubText,

        defMethod' "sendPrivateMessage" $ \_ input -> runE $ sendPrivateMessage input,

        defMethod' "sendShortTextNote" $ \_ input -> runE $ sendShortTextNote input,

        defMethod' "repost" $ \_ eid -> runE $ do
          let eid' = read (unpack eid) :: EventId
          repost eid',

        defMethod' "quoteRepost" $ \_ eid quote -> runE $ do
          let eid' = read (unpack eid) :: EventId
          quoteRepost eid' quote,

        defMethod' "comment" $ \_ eid input -> runE $ do
          let eid' = read (unpack eid) :: EventId
          comment eid' input,

        defMethod' "deleteEvent" $ \_ eid input -> runE $ do
          let eid' = read (unpack eid) :: EventId
          deleteEvent eid' input,

        defMethod' "setCurrentPost" $ \_ meid -> runE $ do
          case meid of
            Just eid -> do
              case readMaybe (unpack eid) of
                Just eid' -> setCurrentPost $ Just eid'
                Nothing -> error $ "Invalid event ID format: " <> show eid
            Nothing -> setCurrentPost Nothing,

        defMethod' "getProfile" $ \_ input -> runE $ do
          case parseNprofileOrNpub input of
            Just (pk, _relayHints) -> do
              profileCache' <- gets @AppState profileCache
              case Map.lookup pk profileCache' of
                Just _ -> do
                  obj <- liftIO $ getPoolObject profilePool pk
                  return $ Just obj
                Nothing -> do
                  profile <- getProfile pk
                  modify @AppState $ \st -> st { profileCache = Map.insert pk profile (profileCache st) }
                  obj <- liftIO $ getPoolObject profilePool pk
                  return $ Just obj
            _ -> return Nothing,

        defMethod' "getPost" $ \_ input -> runE $ do
          let parseEid = case Text.takeWhile (/= '1') input of
                "note"  -> noteToEventId input
                "nevent" -> case neventToEvent input of Just (eid, _, _, _) -> Just eid; Nothing -> Nothing
                "naddr" -> case naddrToEvent input of Just (eid, _, _) -> Just eid; Nothing -> Nothing
                _ -> readMaybe (unpack input) :: Maybe EventId
          --logDebug $ "getPost: " <> pack (show parseEid)
          case parseEid of
            Just eid -> do
              eventExists <- isJust <$> getEvent eid
              if eventExists
              then do
                obj <- liftIO $ getPoolObject postPool eid
                pure $ Just obj
              else pure Nothing
            Nothing -> return Nothing,

        defMethod' "convertNprofileToNpub" $ \_ input -> runE $ do
          case parseNprofileOrNpub input of
            Just (pk, _) -> return $ Just $ pubKeyXOToBech32 pk
            _ -> return Nothing,

        defSignal "downloadCompleted" (Proxy :: Proxy DownloadCompleted),

        defSignal "mediaPeekCompleted" (Proxy :: Proxy MediaPeekCompleted),

        defSignal "mediaCacheCompleted" (Proxy :: Proxy MediaCacheCompleted),

        defMethod' "hasDownload" $ \_ url -> runE $ do
          status <- hasDownload url
          case status of
              Ready (cacheFile, mime, _) -> return ["file:///" <> pack cacheFile, mime]
              _ -> return [],

        defMethod' "peekMimeType" $ \obj url -> runE $ do
          void $ async $ do
              result <- peekMimeType url
              case result of
                  Right mime -> liftIO $ QML.fireSignal (Proxy :: Proxy MediaPeekCompleted) obj url mime
                  Left _ -> pure () -- Could add error signal here if needed
          return (),

        defMethod' "cacheMedia" $ \obj url -> runE $ do
          void $ async $ do
              result <- try @SomeException $ download url
              case result of
                  Left e -> do
                      liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False (pack $ show e)
                  Right status -> case status of
                      Ready (cacheFile, _, _) -> do
                          liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url True ("file:///" <> pack cacheFile)
                      Failed err -> do
                          liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False (pack $ show err)
                      Downloading -> do
                          finalStatus <- waitForDownloadCompletion url
                          case finalStatus of
                              Ready (cacheFile, _, _) -> do
                                  liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url True ("file:///" <> pack cacheFile)
                              Failed err -> do
                                  liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False (pack $ show err)
                              _ -> do
                                  liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False "Unexpected final status"
                      NotStarted -> do
                          liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False "Download not started"
          return (),

        defMethod' "downloadAsync" $ \obj url -> runE $ do
          void $ async $ do
            homeDir <- getHomeDirectory
            let downloadDir = homeDir </> "Downloads"
            createDirectoryIfMissing True downloadDir
            let baseFileName = fromMaybe "downloaded_file" $
                              listToMaybe $ reverse $ Text.splitOn "/" $ Text.takeWhileEnd (/= '?') url
            filePath <- findAvailableFilename downloadDir (unpack baseFileName)
            let fileName = takeFileName filePath

            status <- hasDownload url
            case status of
              Ready (cacheFile, _mime, _expiry) -> do
                result <- try @SomeException $ copyFile cacheFile filePath
                case result of
                  Right _ -> liftIO $ QML.fireSignal (Proxy :: Proxy DownloadCompleted) obj True (pack fileName)
                  Left e -> do
                    --logError $ "Copy from cache failed: " <> pack (show e)
                    liftIO $ QML.fireSignal (Proxy :: Proxy DownloadCompleted) obj False (pack $ show e)
              _ -> do
                result <- try @SomeException $ do
                  r <- liftIO $ Wreq.get (unpack url)
                  let body = r ^. Wreq.responseBody
                  liftIO $ BSL.writeFile filePath body
                  pure filePath
                case result of
                  Right _ -> liftIO $ QML.fireSignal (Proxy :: Proxy DownloadCompleted) obj True (pack fileName)
                  Left (e :: SomeException) -> do
                    --logError $ "Download failed: " <> pack (show e)
                    liftIO $ QML.fireSignal (Proxy :: Proxy DownloadCompleted) obj False (pack $ show e),

        defMethod' "cancelLogin" $ \obj -> runE $ cancelLogin obj
      ]

    newObject rootClass ()


-- Helper function to wait for download completion by polling status
waitForDownloadCompletion :: (Futr :> es, Downloader :> es, Concurrent :> es) => Text -> Eff es DownloadStatus
waitForDownloadCompletion url = do
    status <- hasDownload url
    case status of
        Downloading -> do
            threadDelay 100000  -- 100ms
            waitForDownloadCompletion url
        _ -> return status


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
