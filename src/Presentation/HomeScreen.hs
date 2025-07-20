{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Presentation.HomeScreen where

import Control.Lens ((^.))
import Control.Monad (filterM, void)
import Data.Aeson (decode, encode, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), defConfig, keyOrder)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effectful
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
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)

import Downloader (DownloadStatus(..), hasDownload, peekMimeType, download)
import KeyMgmt (AccountId(..), updateProfile)
import Logging
import Nostr
import Nostr.Bech32 (pubKeyXOToBech32, eventToNevent, eventIdToNote, bech32ToPubKeyXO, noteToEventId, neventToEvent, naddrToEvent)
import Nostr.Event (Event(..), EventId(..), createMetadata, getEventId)
import Nostr.Event qualified as NE
import Nostr.Publisher
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.ProfileManager (fetchProfile, getProfile)
import Nostr.Util
import Presentation.Classes qualified as Classes
import Presentation.Classes (findAvailableFilename)
import Presentation.KeyMgmtUI qualified as KeyMgmtUI
import Presentation.RelayMgmtUI qualified as RelayMgmtUI
import Futr hiding (Comment, QuoteRepost, Repost)
import Futr (MediaPeekCompleted, MediaCacheCompleted)
import Store.Lmdb (LmdbStore, getEvent, getFollows, getEventRelays)
import Types (Post(..), AppState(..), AppScreen(..), RelayPool(..), FeedFilter(..), Follow(..))


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

    followClass' <- runE $ Classes.followClass changeKey'
    profileClass' <- runE $ Classes.profileClass changeKey'
    postClass' <- runE $ Classes.postClass changeKey'
    publishStatusClass' <- runE $ Classes.publishStatusClass changeKey'

    followPool <- newFactoryPool (newObject followClass')
    profilesPool <- newFactoryPool (newObject profileClass')
    postsPool <- newFactoryPool (newObject postClass')
    publishStatusPool <- newFactoryPool (newObject publishStatusClass')

    rootClass <- newClass [
        defPropertyConst' "version" (\_ -> do
          st <- runE $ get @AppState
          return $ version st
        ),

        defPropertyConst' "ctxKeyMgmt" (\_ -> do
          return keyMgmtObj
        ),

        defPropertyConst' "ctxRelayMgmt" (\_ -> do
          return relayMgmtObj
        ),

        defPropertyConst' "currentProfile" (\_ -> do
          mp <- runE $ gets @AppState currentProfile
          case mp of
            Just (pk, _) -> do
              profileObj <- getPoolObject profilesPool pk
              return $ Just profileObj
            Nothing -> return Nothing),

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

        defPropertySigRO' "mynpub" changeKey' $ \_ -> do
          kp <- runE $ getKeyPair
          return $ pubKeyXOToBech32 $ keyPairToPubKeyXO kp,

        defPropertySigRO' "mypicture" changeKey' $ \_ -> runE $ do
          kp <- getKeyPair
          (profile, _) <- getProfile $ keyPairToPubKeyXO kp
          return $ fromMaybe "" $ picture profile,

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

        defMethod' "loadFeed" $ \(_ :: ObjRef ()) (feedType :: Text) (input :: Text) -> runE $ do
          let f = case feedType of
                "public" -> case bech32ToPubKeyXO input of
                  Just pubKeyXO -> PostsFilter pubKeyXO
                  Nothing -> error "Invalid bech32 public key"
                "private" -> case bech32ToPubKeyXO input of
                  Just pubKeyXO -> PrivateMessagesFilter pubKeyXO
                  Nothing -> error "Invalid bech32 public key"
                _ -> error "Invalid feed type"
          void $ loadFeed f
          ck <- gets @QtQuickState signalKey
          let ck' = fromMaybe (error "Signal key not found") ck
          feedClass' <- Classes.feedClass postsPool ck'
          feedObj <- liftIO $ newObject feedClass' ()
          return (feedObj :: ObjRef ()),

        defMethod' "saveProfile" $ \_ input -> runE $ do
            let profile = maybe (error "Invalid profile JSON") id $ decode (BSL.fromStrict $ TE.encodeUtf8 input) :: Profile
            n <- getCurrentTime
            kp <- getKeyPair
            let unsigned = createMetadata profile (keyPairToPubKeyXO kp) n
            signedMaybe <- signEvent unsigned kp
            case signedMaybe of
              Just signed -> do
                broadcast signed
                let aid = AccountId $ pubKeyXOToBech32 (keyPairToPubKeyXO kp)
                updateProfile aid profile
              Nothing -> logWarning "Failed to sign profile update event",

        defPropertySigRO' "followList" changeKey' $ \obj -> do
          runE $ modify $ \s -> s { uiRefs = (uiRefs s) { followsObjRef = Just obj } }
          kp <- runE $ getKeyPair
          let userPubKey = keyPairToPubKeyXO kp
          followedPubKeys <- runE $ getFollows userPubKey
          mapM (getPoolObject followPool) (userPubKey : map pubkey followedPubKeys),

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

        defMethod' "follow" $ \_ npubText -> do
          runE $ followProfile npubText,

        defMethod' "unfollow" $ \_ npubText -> do
          runE $ unfollowProfile npubText,

        defMethod' "sendPrivateMessage" $ \_ input -> do
          runE $ sendPrivateMessage input,

        defMethod' "sendShortTextNote" $ \_ input -> do
          runE $ sendShortTextNote input,

        defMethod' "repost" $ \_ eid -> do
          runE $ do
            let eid' = read (unpack eid) :: EventId
            repost eid',

        defMethod' "quoteRepost" $ \_ eid quote -> do
          runE $ do
            let eid' = read (unpack eid) :: EventId
            quoteRepost eid' quote,

        defMethod' "comment" $ \_ eid input -> do
          runE $ do
            let eid' = read (unpack eid) :: EventId
            comment eid' input,

        defMethod' "deleteEvent" $ \_ eid input -> do
          runE $ do
            let eid' = read (unpack eid) :: EventId
            deleteEvent eid' input,

        defMethod' "setCurrentPost" $ \_ meid -> do
          runE $ do
            case meid of
              Just eid -> do
                case readMaybe (unpack eid) of
                  Just eid' -> setCurrentPost $ Just eid'
                  Nothing -> logError $ "Invalid event ID format: " <> eid
              Nothing -> setCurrentPost Nothing,

        defMethod' "getProfile" $ \_ input -> do
          result <- case parseNprofileOrNpub input of
            Just (pk, relayHints) -> do
              _ <- runE $ fetchProfile pk relayHints  -- this will return profile and handle fetch if needed
              profileObj <- getPoolObject profilesPool pk
              return $ Just profileObj
            _ -> return Nothing
          return result,

        defMethod' "getPost" $ \_ input -> do
          runE $ do
            let fetchByEventId eid = do
                  eventMaybe <- getEvent eid
                  case eventMaybe of
                    Just event -> do
                      post <- Classes.createPost event
                      -- Flatten post like in Classes.hs
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
                          maybe "" eventIdToNote (referencedPostId post)        -- referencedPostId
                          ]
                    Nothing -> return []

            let fetchByType parseFunc = case parseFunc input of
                    Just eid -> fmap Just (fetchByEventId eid)
                    Nothing -> return Nothing

            case Text.takeWhile (/= '1') input of
                "note"  -> fetchByType noteToEventId
                "nevent" -> fetchByType (\i -> case neventToEvent i of Just (eid, _, _, _) -> Just eid; Nothing -> Nothing)
                "naddr" -> fetchByType (\i -> case naddrToEvent i of Just (eid, _, _) -> Just eid; Nothing -> Nothing)
                _ -> do
                  let meid = readMaybe (unpack input) :: Maybe EventId
                  case meid of
                    Just eid -> fmap Just (fetchByEventId eid)
                    Nothing -> return Nothing,

        defMethod' "convertNprofileToNpub" $ \_ input -> do
          runE $ do
            case parseNprofileOrNpub input of
              Just (pk, _) -> return $ Just $ pubKeyXOToBech32 pk
              _ -> return Nothing,

        defSignal "downloadCompleted" (Proxy :: Proxy DownloadCompleted),
        
        defSignal "mediaPeekCompleted" (Proxy :: Proxy MediaPeekCompleted),
        
        defSignal "mediaCacheCompleted" (Proxy :: Proxy MediaCacheCompleted),

        defMethod' "hasDownload" $ \_ url -> runE $ do
            status <- hasDownload url
            case status of
                Ready (cacheFile, mime, _) -> return [pack cacheFile, mime]
                _ -> return [],

        defMethod' "peekMimeType" $ \obj url -> runE $ do
            void $ async $ do
                result <- peekMimeType url
                case result of
                    Right mime -> liftIO $ QML.fireSignal (Proxy :: Proxy MediaPeekCompleted) obj url mime
                    Left _ -> pure () -- Could add error signal here if needed
            return (),

        defMethod' "cacheMedia" $ \obj url -> runE $ do
            logInfo $ "*** Caching media: ORIGIN: " <> url
            void $ async $ do
                logInfo $ "*** Caching media ASYNC: " <> url
                result <- try @SomeException $ download url  -- This caches the media
                case result of
                    Left e -> do
                        logError $ "*** Exception during download: " <> pack (show e)
                        liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False (pack $ show e)
                    Right status -> case status of
                        Ready (cacheFile, mime, _) -> do
                            logInfo $ "*** Cached media: OK"
                            liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url True ("file:///" <> pack cacheFile)
                        Failed err -> do
                            logError $ "*** Failed to cache media: " <> pack (show err)
                            liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False (pack $ show err)
                        _ -> do
                            logError $ "*** Unexpected download status: " <> pack (show status)
                            liftIO $ QML.fireSignal (Proxy :: Proxy MediaCacheCompleted) obj url False "Unexpected status"
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
                      logError $ "Copy from cache failed: " <> pack (show e)
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
                      logError $ "Download failed: " <> pack (show e)
                      liftIO $ QML.fireSignal (Proxy :: Proxy DownloadCompleted) obj False (pack $ show e),

        defMethod' "cancelLogin" $ \obj -> runE $ cancelLogin obj
      ]

    newObject rootClass ()


-- Helper function to fetch and create an event object
fetchEventObject :: (LmdbStore :> es, IOE :> es) => FactoryPool EventId -> EventId -> Eff es (Maybe (ObjRef EventId))
fetchEventObject pool eid = do
    eventMaybe <- getEvent eid
    case eventMaybe of
        Just _ -> do
            obj <- liftIO (getPoolObject pool eid)
            return (Just obj)
        Nothing -> return Nothing
