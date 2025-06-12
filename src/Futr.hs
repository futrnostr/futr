{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Futr where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (ToJSON, pairs, toEncoding, (.=))
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Set qualified as Set
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically, flushTQueue, newTVarIO, readTQueue, readTVar, writeTVar)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Exception (SomeException, try)
import Effectful.FileSystem
  ( FileSystem,
    XdgDirectory (XdgData),
    createDirectoryIfMissing,
    getXdgDirectory
  )
import Effectful.State.Static.Shared (State, get, gets, modify, put)
import Lmdb.Connection (closeEnvironment)
import QtQuick
import GHC.Generics (Generic)
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Graphics.QML qualified as QML
import System.FilePath ((</>))

import Logging
import KeyMgmt (Account(..), AccountId(..), KeyMgmt, KeyMgmtState(..))
import Nostr
import Nostr.Bech32
import Nostr.Event ( Event(..), EventId, UnsignedEvent(..)
                   , createComment, createEventDeletion, createFollowList
                   , createRepost, createRumor, createShortTextNote, eventIdToHex
                   )
import Nostr.Event qualified as NE
import Nostr.EventHandler (EventHandler, handleEvent)
import Nostr.InboxModel ( InboxModel, awaitAtLeastOneConnected, startInboxModel
                        , stopInboxModel, subscribeToProfilesAndPostsFor
                        , subscribeToCommentsFor, unsubscribeToCommentsFor )
import Nostr.Keys (PubKeyXO, derivePublicKeyXO, keyPairToPubKeyXO, pubKeyXOToHex, secKeyToKeyPair)
import Nostr.Nip05Search (isNip05Identifier, searchNip05, Nip05SearchResult(..))
import Nostr.ProfileManager (ProfileManager)
import Nostr.Publisher
import Nostr.Relay (RelayURI, getUri, isInboxCapable, isValidRelayURI)
import Nostr.RelayConnection (RelayConnection, connect, disconnect)
import Nostr.Subscription
import Nostr.SubscriptionHandler (SubscriptionHandler)
import Nostr.Util
import Presentation.Classes (Classes)
import Presentation.KeyMgmtUI (KeyMgmtUI)
import Presentation.RelayMgmtUI (RelayMgmtUI)
import RelayMgmt (RelayMgmt)
import Store.Lmdb ( LmdbState(..), LmdbStore, initialLmdbState, initializeLmdbState
                  , getEvent, getEventRelays, getFollows, getGeneralRelays )
import Types

-- | Signal key class for LoginStatusChanged.
data LoginStatusChanged deriving Typeable

instance SignalKeyClass LoginStatusChanged where
    type SignalParams LoginStatusChanged = Bool -> Text -> IO ()


-- | Signal key class for DownloadCompleted.
data DownloadCompleted deriving Typeable

instance SignalKeyClass DownloadCompleted where
    type SignalParams DownloadCompleted = Bool -> Text -> IO ()


-- | Search result.
data SearchResult
  = ProfileResult { npub :: Text, relayUris :: [Text] }
  | Nip05Result { npub :: Text, nip05 :: Text, relayUris :: [Text] }
  | NoResult
  deriving (Eq, Generic, Show)

instance ToJSON SearchResult where
  toEncoding (ProfileResult npub' relayUris') = pairs
     ( "npub"  .= npub'
    <> "relayUris" .= relayUris'
     )
  toEncoding (Nip05Result npub' nip05' relayUris') = pairs
     ( "npub" .= npub'
    <> "nip05" .= nip05'
    <> "relayUris" .= relayUris'
     )
  toEncoding NoResult = pairs ( "result" .= ("no_result" :: Text) )


-- | Futr Effects.
data Futr :: Effect where
  Login :: ObjRef () -> Text -> Futr m ()
  Search :: ObjRef () -> Text -> Futr m SearchResult
  SetCurrentPost :: Maybe EventId -> Futr m ()
  FollowProfile :: Text -> Futr m ()
  UnfollowProfile :: Text -> Futr m ()
  LoadFeed :: PubKeyXO -> Futr m ()
  SendPrivateMessage :: Text -> Futr m ()
  SendShortTextNote :: Text -> Futr m ()
  Logout :: ObjRef () -> Futr m ()
  Repost :: EventId -> Futr m ()
  QuoteRepost :: EventId -> Text -> Futr m ()
  Comment :: EventId -> Text -> Futr m ()
  DeleteEvent :: EventId -> Text -> Futr m ()
  CancelLogin :: ObjRef () -> Futr m ()


-- | Dispatch type for Futr effect.
type instance DispatchOf Futr = Dynamic


login :: Futr :> es => ObjRef () -> Text -> Eff es ()
login obj input = send $ Login obj input

search :: Futr :> es => ObjRef () -> Text -> Eff es SearchResult
search obj input = send $ Search obj input

setCurrentPost :: Futr :> es => Maybe EventId -> Eff es ()
setCurrentPost eid = send $ SetCurrentPost eid

followProfile :: Futr :> es => Text -> Eff es ()
followProfile npub' = send $ FollowProfile npub'

unfollowProfile :: Futr :> es => Text -> Eff es ()
unfollowProfile npub' = send $ UnfollowProfile npub'

loadFeed :: Futr :> es => PubKeyXO -> Eff es ()
loadFeed pk = send $ LoadFeed pk

sendPrivateMessage :: Futr :> es => Text -> Eff es ()
sendPrivateMessage input = send $ SendPrivateMessage input

sendShortTextNote :: Futr :> es => Text -> Eff es ()
sendShortTextNote input = send $ SendShortTextNote input

logout :: Futr :> es => ObjRef () -> Eff es ()
logout obj = send $ Logout obj

repost :: Futr :> es => EventId -> Eff es ()
repost eid = send $ Repost eid

quoteRepost :: Futr :> es => EventId -> Text -> Eff es ()
quoteRepost eid quote = send $ QuoteRepost eid quote

comment :: Futr :> es => EventId -> Text -> Eff es ()
comment eid comment' = send $ Comment eid comment'

deleteEvent :: Futr :> es => EventId -> Text -> Eff es ()
deleteEvent eid reason = send $ DeleteEvent eid reason

cancelLogin :: Futr :> es => ObjRef () -> Eff es ()
cancelLogin obj = send $ CancelLogin obj


-- | Effectful type for Futr.
type FutrEff es =
  ( State AppState :> es
  , State LmdbState :> es
  , LmdbStore :> es
  , EventHandler :> es
  , KeyMgmt :> es
  , KeyMgmtUI :> es
  , RelayMgmtUI :> es
  , Nostr :> es
  , ProfileManager :> es
  , InboxModel :> es
  , RelayConnection :> es
  , RelayMgmt :> es
  , Subscription :> es
  , SubscriptionHandler :> es
  , Publisher :> es
  , State KeyMgmtState :> es
  , State RelayPool :> es
  , State QtQuickState :> es
  , QtQuick :> es
  , Classes :> es
  , Logging :> es
  , IOE :> es
  , FileSystem :> es
  , Concurrent :> es
  , Util :> es
  )


-- | Run the Futr effect.
runFutr :: FutrEff es => Eff (Futr : es) a -> Eff es a
runFutr = interpret $ \_ -> \case
  Login obj input -> do
      kst <- get @KeyMgmtState
      case Map.lookup (AccountId input) (accountMap kst) of
        Nothing -> do
          logError $ "Account not found: " <> input
          return ()
        Just a -> do
          let pk = accountPubKeyXO a

          dbResult <- try @SomeException $ do
              baseDir <- getXdgDirectory XdgData ("futrnostr" </> unpack (pubKeyXOToBech32 pk))
              let lmdbDir = baseDir </> "db"
              createDirectoryIfMissing True lmdbDir
              lmdbState <- liftIO $ initializeLmdbState lmdbDir
              put @LmdbState lmdbState

          case dbResult of
            Left e -> do
              logError $ "Database initialization failed: " <> pack (show e)
              liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Database initialization failed"
              return ()
            Right _ -> do
              loginWithAccount obj a

  Search _ input -> do
    st <- get @AppState
    let myPubKey = keyPairToPubKeyXO <$> keyPair st

    -- Check if input is a NIP-05 identifier (email-like)
    if isNip05Identifier input
      then do
        -- Perform NIP-05 search
        nip05Result <- liftIO $ searchNip05 input
        case nip05Result of
          Just (Nip05SearchResult nip05Id userPubKey relayHints) -> do
            let npubStr = pubKeyXOToBech32 userPubKey
            -- Check if it's the current user
            if Just userPubKey == myPubKey
              then return $ Nip05Result npubStr nip05Id relayHints
              else do
                -- Check if already following
                currentFollows <- maybe [] id <$> traverse getFollows myPubKey
                if any (\f -> pubkey f == userPubKey) currentFollows
                  then return $ Nip05Result npubStr nip05Id relayHints
                  else do
                    -- Search in relays to get more info about the user
                    searchInRelays userPubKey relayHints
                    return $ Nip05Result npubStr nip05Id relayHints
          Nothing -> return NoResult
      -- Check for nprofile/npub format
      else if "nprofile" `isPrefixOf` input || "npub" `isPrefixOf` input
        then case parseNprofileOrNpub input of
          Nothing -> return NoResult
          Just (pubkey', relayUris)
            | Just pubkey' == myPubKey -> return $ ProfileResult (pubKeyXOToBech32 pubkey') relayUris
            | otherwise -> do
                currentFollows <- maybe [] id <$> traverse getFollows myPubKey
                if any (\f -> pubkey f == pubkey') currentFollows
                  then return $ ProfileResult (pubKeyXOToBech32 pubkey') relayUris
                  else do
                    searchInRelays pubkey' relayUris
                    return $ ProfileResult (pubKeyXOToBech32 pubkey') relayUris
        else return NoResult

  SetCurrentPost eid -> do
    previousPost <- gets @AppState currentPost

    modify @AppState $ \st -> st { currentPost = eid }

    case (eid, previousPost) of
      (Just newId, Just oldId) -> do
        if newId == oldId
          then pure()
          else do
            unsubscribeToCommentsFor oldId
            subscribeToCommentsFor newId
      (Just newId, Nothing) -> do
        subscribeToCommentsFor newId
      (Nothing, Just oldId) -> do
        clearCommentsRef
        unsubscribeToCommentsFor oldId
      (Nothing, Nothing) -> do
        clearCommentsRef
    where
      clearCommentsRef =
        modify @QtQuickState $ \st ->
          st { uiRefs = (uiRefs st) { currentPostCommentsObjRef = Nothing } }

  FollowProfile npub' -> do
    let targetPK = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    case keyPairToPubKeyXO <$> keyPair st of
        Just userPK -> do
            currentFollows <- getFollows userPK
            unless (targetPK `elem` map pubkey currentFollows) $ do
                let newFollow = Follow targetPK Nothing
                    newFollows = newFollow : currentFollows
                sendFollowListEvent newFollows
                notify $ emptyUpdates { myFollowsChanged = True }
        Nothing -> return ()

  UnfollowProfile npub' -> do
    let targetPK = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    case keyPairToPubKeyXO <$> keyPair st of
        Just userPK -> do
            currentFollows <- getFollows userPK
            let newFollows = filter ((/= targetPK) . pubkey) currentFollows
            sendFollowListEvent newFollows
            notify $ emptyUpdates { myFollowsChanged = True }
        Nothing -> return ()

  LoadFeed pk -> do
    st <- get @AppState

    case currentProfile st of
      Just (_, subIds) -> forM_ subIds $ \subId' -> stopSubscription subId'
      _ -> return ()

    modify @AppState $ \st' -> st' { currentProfile = Just (pk, []) }
    notify $ emptyUpdates { postsChanged = True, privateMessagesChanged = True }

    void $ async $ do
      kp <- getKeyPair
      let mypk = keyPairToPubKeyXO kp
      follows <- getFollows mypk

      when (not (pk `elem` map pubkey follows)) $ do
        subIds <- subscribeToProfilesAndPostsFor pk
        modify @AppState $ \st' -> st' { currentProfile = Just (pk, subIds) }

  SendPrivateMessage input -> do
    st <- get @AppState
    case (keyPair st, currentProfile st) of
      (Just kp, Just (recipient, _)) -> do
        now <- getCurrentTime
        let senderPubKeyXO = keyPairToPubKeyXO kp
            allRecipients = nub [senderPubKeyXO, recipient]
            rumor = createRumor senderPubKeyXO now (map (\xo -> ["p", pubKeyXOToHex xo]) [recipient]) input

        giftWraps <- forM allRecipients $ \recipient' -> do
          seal <- createSeal rumor kp recipient'
          case seal of
            Just seal' -> do
              giftWrapResult <- createGiftWrap seal' recipient'
              case giftWrapResult of
                Just (gw, _) -> return (Just gw)
                Nothing -> logError "Failed to create gift wrap" >> return Nothing
            Nothing -> logError "Failed to create seal" >> return Nothing

        let validGiftWraps = catMaybes giftWraps

        forM_ validGiftWraps $ \gw -> do
          publishGiftWrap gw senderPubKeyXO recipient
        notify $ emptyUpdates { privateMessagesChanged = True }

      (Nothing, _) -> logError "No key pair found"
      (_, Nothing) -> logError "No current chat recipient"

  SendShortTextNote input -> do
    kp <- getKeyPair
    now <- getCurrentTime
    let u = createShortTextNote input (keyPairToPubKeyXO kp) now
    signed <- signEvent u kp
    case signed of
      Just s -> do
        publishToOutbox s
        notify $ emptyUpdates { postsChanged = True }
      Nothing -> logError "Failed to sign short text note"

  Logout obj -> do
    fullAppCleanup
    logInfo "User logged out successfully"
    fireSignal obj

  Repost eid -> do
    kp <- getKeyPair
    now <- getCurrentTime
    mEvent <- getEvent eid
    case mEvent of
      Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
      Just event -> do
        relays <- getEventRelays eid
        let e = createRepost event (Set.findMin relays) (keyPairToPubKeyXO kp) now
        signed <- signEvent e kp
        case signed of
          Nothing -> logError "Failed to sign repost"
          Just s -> do
            publishToOutbox s
            mEventAndRelays <- getEvent eid
            case mEventAndRelays of
              Nothing -> return ()
              Just origEvent -> do
                relaySet <- getEventRelays eid
                let eventRelayUris = Set.fromList $
                      [ uri | ("r":uri:_) <- tags origEvent
                      , isValidRelayURI uri
                      ]

                authorRelays <- getGeneralRelays (pubKey origEvent)
                let authorInboxUris = Set.fromList $ map getUri $
                      filter isInboxCapable authorRelays

                let targetUris = eventRelayUris `Set.union` authorInboxUris `Set.union` relaySet

                forM_ (Set.toList targetUris) $ \relay ->
                  publishToRelay s relay
                notify $ emptyUpdates { postsChanged = True }

  QuoteRepost eid quote -> do
    kp <- getKeyPair
    now <- getCurrentTime
    mEvent <- getEvent eid
    case mEvent of
      Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
      Just event -> do
        relays <- getEventRelays $ eventId event
        let q = createQuoteRepost event (Set.findMin relays) quote (keyPairToPubKeyXO kp) now
        signed <- signEvent q kp
        case signed of
          Nothing -> logError "Failed to sign quote repost"
          Just s -> do
            publishToOutbox s
            notify $ emptyUpdates { postsChanged = True }
    where
      createQuoteRepost :: Event -> RelayURI -> Text -> PubKeyXO -> Int -> UnsignedEvent
      createQuoteRepost event relayUrl quote' xo t =
        UnsignedEvent
          { pubKey' = xo
          , createdAt' = t
          , kind' = NE.ShortTextNote
          , tags' = [ ["q", eventIdToHex $ eventId event, relayUrl, pubKeyXOToHex $ pubKey event]
                    ]
          , content' = quote' <> "\n\nnostr:" <> eventToNevent event [relayUrl]
          }

  Comment eid comment' -> do
    kp <- getKeyPair
    now <- getCurrentTime
    mEvent <- getEvent eid
    case mEvent of
      Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
      Just ev -> do
        let c = createComment ev comment' (keyPairToPubKeyXO kp) now
        signed <- signEvent c kp
        case signed of
          Nothing -> logError "Failed to sign comment"
          Just s -> do
            publishToOutbox s
            mEventAndRelays <- getEvent eid
            case mEventAndRelays of
              Just origEvent -> do
                relaySet <- getEventRelays eid
                let eventRelayUris = Set.fromList $
                      [ uri | ("r":uri:_) <- tags origEvent
                      , isValidRelayURI uri
                      ]

                authorRelays <- getGeneralRelays (pubKey origEvent)
                let authorInboxUris = Set.fromList $ map getUri $
                      filter isInboxCapable authorRelays

                let targetUris = eventRelayUris `Set.union` authorInboxUris `Set.union` relaySet

                forM_ (Set.toList targetUris) $ \relay ->
                  publishToRelay s relay
                notify $ emptyUpdates { postsChanged = True }
              Nothing -> return ()

  Futr.DeleteEvent eid reason -> do
      kp <- getKeyPair
      now <- getCurrentTime
      mEvent <- getEvent eid
      case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just ev -> do
              let deletion = createEventDeletion ev reason (keyPairToPubKeyXO kp) now
              signed <- signEvent deletion kp
              case signed of
                  Nothing -> logError "Failed to sign event deletion"
                  Just s -> do
                      publishToOutbox s
                      notify $ emptyUpdates { postsChanged = True, privateMessagesChanged = True }

  CancelLogin obj -> do
    fullAppCleanup
    liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Login cancelled"
    logInfo "Login cancelled"
    fireSignal obj


-- Helper function to parse nprofile or npub
parseNprofileOrNpub :: Text -> Maybe (PubKeyXO, [RelayURI])
parseNprofileOrNpub input =
  case bech32ToPubKeyXO input of
    Just pubkey' -> Just (pubkey', [])  -- for npub
    Nothing -> nprofileToPubKeyXO input  -- for nprofile


-- | Login with an account.
loginWithAccount :: FutrEff es => ObjRef () -> Account -> Eff es ()
loginWithAccount obj a = do
    modify @AppState $ \s -> s { keyPair = Just (secKeyToKeyPair $ accountSecKey a) }
    modify @KeyMgmtState $ \st -> st
        { nsecView = secKeyToBech32 $ accountSecKey a
        , npubView = pubKeyXOToBech32 $ derivePublicKeyXO $ accountSecKey a
        }

    void $ async $ do
      startInboxModel
      atLeastOneConnected <- awaitAtLeastOneConnected

      if atLeastOneConnected
        then do
          modify @AppState $ \s -> s { currentScreen = Home }
          liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj True ""
        else do
          liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Failed to connect to any relay"

      fireSignal obj

-- | Send a follow list event.
sendFollowListEvent :: FutrEff es => [Follow] -> Eff es ()
sendFollowListEvent follows = do
    kp <- getKeyPair
    let userPK = keyPairToPubKeyXO kp
    currentTime <- getCurrentTime
    let followTuples = map (\f -> (pubkey f, Nothing)) follows
    let event = createFollowList followTuples userPK currentTime
    signedEvent <- signEvent event kp
    case signedEvent of
        Just signedEvent' -> do
            publishToOutbox signedEvent'
        Nothing -> logError "Failed to sign follow list event"


-- | Search for a profile in relays.
searchInRelays :: FutrEff es => PubKeyXO -> [RelayURI] -> Eff es ()
searchInRelays xo relayUris = do
    manuallyConnected <- case relayUris of
        [relayUri] -> do
            conns <- gets @RelayPool activeConnections
            if Map.member relayUri conns
                then return False
                else do
                    void $ connect relayUri
                    return True
        _ -> return False

    relays <- getGeneralRelays xo
    conns <- gets @RelayPool activeConnections

    let searchRelays = case relayUris of
            [relayUri] -> [relayUri]
            _ -> map getUri relays

    forM_ searchRelays $ \relayUri' -> do
        when (Map.member relayUri' conns) $ do
            subId' <- subscribe relayUri' (metadataFilter [xo])
            subs <- gets @RelayPool subscriptions
            let q = case Map.lookup subId' subs of
                      Just sub -> responseQueue sub
                      Nothing -> error $ "Subscription " <> show subId' <> " not found"
            -- @todo duplicated from subscription handler, but closes unneeded connections

            void $ async $ do
                shouldStopVar <- newTVarIO False
                let loop = do
                        e <- atomically $ readTQueue q
                        es <- atomically $ flushTQueue q

                        forM_ (e:es) $ \(relayUri, e') -> do
                            case e' of
                                EventAppeared event' -> handleEvent (Just relayUri) event'

                                SubscriptionEose _ -> do
                                  stopSubscription subId'
                                  when (manuallyConnected && relayUri' `elem` relayUris) $ do
                                    disconnect relayUri'

                                SubscriptionClosed _ -> do
                                  atomically $ writeTVar shouldStopVar True
                                  when (manuallyConnected && relayUri' `elem` relayUris) $ do
                                    disconnect relayUri'

                        shouldStop <- atomically $ readTVar shouldStopVar
                        unless shouldStop loop

                loop


-- | Helper to fully clean up app state, disconnect all relays, and fire signal
fullAppCleanup :: FutrEff es => Eff es ()
fullAppCleanup = do
  stopInboxModel

  relayPool <- get @RelayPool
  let relayUris = Map.keys (activeConnections relayPool)
  forM_ relayUris disconnect

  threadDelay 100000

  st <- get @LmdbState
  liftIO $ closeEnvironment (lmdbEnv st)

  put @LmdbState initialLmdbState
  put @AppState initialState
  put @RelayPool initialRelayPool
