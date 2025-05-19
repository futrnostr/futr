{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Presentation.HomeScreen where

import Control.Exception (try, SomeException)
import Control.Lens ((^.))
import Control.Monad (filterM, void)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem
import Effectful.State.Static.Shared (get, gets, modify)
import QtQuick
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Graphics.QML qualified as QML
import Network.Wreq qualified as Wreq
import Prelude hiding (drop)
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)

import KeyMgmt (AccountId(..), updateProfile)
import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (Event(..), EventId(..), createMetadata)
import Nostr.Publisher
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.ProfileManager (fetchProfile, getProfile)
import Nostr.Util
import Presentation.Classes
import Presentation.KeyMgmtUI qualified as KeyMgmtUI
import Presentation.RelayMgmtUI qualified as RelayMgmtUI
import Futr hiding (Comment, QuoteRepost, Repost)
import Store.Lmdb (LmdbStore, TimelineType(..), getEvent, getFollows, getTimelineIds)
import Types


-- | Key Management Effect for creating QML UI.
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

    followClass' <- runE $ followClass changeKey'
    profileClass' <- runE $ profileClass changeKey'
    postClass' <- runE $ postClass changeKey'
    publishStatusClass' <- runE $ publishStatusClass changeKey'

    followPool <- newFactoryPool (newObject followClass')
    profilesPool <- newFactoryPool (newObject profileClass')
    postsPool <- newFactoryPool (newObject postClass')
    chatPool <- newFactoryPool (newObject postClass')
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
                runE $ logDebug $ "Setting property: currentScreen = " <> newScreen
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        runE $ do
                          modify $ \st -> st { currentScreen = s }
                          fireSignal obj
                    Nothing -> return ()),

        defPropertySigRO' "mynpub" changeKey' $ \_ -> do
          kp <- runE $ getKeyPair
          return $ pubKeyXOToBech32 $ keyPairToPubKeyXO kp,

        defPropertySigRO' "mypicture" changeKey' $ \_ -> do
          kp <- runE $ getKeyPair
          (profile, _) <- runE $ getProfile $ keyPairToPubKeyXO kp
          return $ fromMaybe "" $ picture profile,

        defPropertySigRO' "inboxModelState" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { inboxModelStateObjRef = Just obj } }
          st <- runE $ get @AppState
          return $ pack $ show $ inboxModelState st,

        defSignal "loginStatusChanged" (Proxy :: Proxy LoginStatusChanged),

        defMethod' "login" $ \obj input -> do
          runE $ login obj input,

        defMethod' "logout" $ \obj -> do
          runE $ logout obj,

        defMethod' "search" $ \obj input -> do
          runE $ do
            res <- search obj input
            return $ TE.decodeUtf8 $ BSL.toStrict $ encode res,

        defMethod' "saveProfile" $ \_ input -> do
          runE $ do
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

        defPropertySigRO' "posts" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { postsObjRef = Just obj } }
          st <- runE $ get @AppState
          case currentProfile st of
            Just (recipient, _) -> do
                postIds <- runE $ getTimelineIds PostTimeline recipient 1000
                mapM (getPoolObject postsPool) postIds
            Nothing -> return [],

        defPropertySigRO' "privateMessages" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { privateMessagesObjRef = Just obj } }
          st <- runE $ get @AppState
          case currentProfile st of
            Just (recipient, _) -> do
                messageIds <- runE $ getTimelineIds ChatTimeline recipient 1000
                mapM (getPoolObject chatPool) messageIds
            Nothing -> return [],

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

        defMethod' "loadFeed" $ \_ npubText -> do
          runE $ do
              let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npubText
              loadFeed pubKeyXO,

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
            let fetchByType parseFunc = case parseFunc input of
                    Just eid -> fetchEventObject postsPool eid
                    Nothing -> return Nothing

            case Text.takeWhile (/= '1') input of
                "note"  -> fetchByType noteToEventId
                "nevent" -> fetchByType (\i -> case neventToEvent i of Just (eid, _, _, _) -> Just eid; Nothing -> Nothing)
                "naddr" -> fetchByType (\i -> case naddrToEvent i of Just (eid, _, _) -> Just eid; Nothing -> Nothing)
                _ -> do
                  let meid = readMaybe (unpack input) :: Maybe EventId
                  case meid of
                    Just eid -> do
                      fetchEventObject postsPool eid
                    Nothing -> return Nothing,

        defMethod' "convertNprofileToNpub" $ \_ input -> do
          runE $ do
            case parseNprofileOrNpub input of
              Just (pk, _) -> return $ Just $ pubKeyXOToBech32 pk
              _ -> return Nothing,

        defSignal "downloadCompleted" (Proxy :: Proxy DownloadCompleted),

        defMethod' "downloadAsync" $ \obj url -> do
          runE $ do
            -- Start the download asynchronously
            void $ async $ do
              homeDir <- getHomeDirectory
              let downloadDir = homeDir </> "Downloads"
              createDirectoryIfMissing True downloadDir
              let baseFileName = fromMaybe "downloaded_file" $
                                listToMaybe $ reverse $ Text.splitOn "/" $ Text.takeWhileEnd (/= '?') url
              filePath <- findAvailableFilename downloadDir (unpack baseFileName)
              let fileName = takeFileName filePath

              result <- liftIO $ try $ do
                r <- Wreq.get (unpack url)
                let body = r ^. Wreq.responseBody
                BSL.writeFile filePath body
                return filePath

              -- Fire the signal with the result
              case result of
                Right _ -> do
                  liftIO $ QML.fireSignal (Proxy :: Proxy DownloadCompleted) obj True (pack fileName)
                Left (e :: SomeException) -> do
                  logError $ "Download failed: " <> pack (show e)
                  liftIO $ QML.fireSignal (Proxy :: Proxy DownloadCompleted) obj False (pack $ show e)

            return ()
      ]

    rootObj <- newObject rootClass ()

    return rootObj


-- Helper function to fetch and create an event object
fetchEventObject :: (LmdbStore :> es, IOE :> es) => FactoryPool EventId -> EventId -> Eff es (Maybe (ObjRef EventId))
fetchEventObject pool eid = do
    eventMaybe <- getEvent eid
    case eventMaybe of
        Just _ -> do
            obj <- liftIO (getPoolObject pool eid)
            return (Just obj)
        Nothing -> return Nothing
