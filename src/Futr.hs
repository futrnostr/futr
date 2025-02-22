{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Futr where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (ToJSON, pairs, toEncoding, (.=))
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Set qualified as Set
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically, flushTQueue, newTQueueIO, newTVarIO, readTQueue, readTVar, writeTVar)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (SomeException, try)
import Effectful.FileSystem
  ( FileSystem,
    XdgDirectory (XdgData),
    createDirectoryIfMissing,
    getXdgDirectory
  )
import Effectful.State.Static.Shared (State, get, gets, modify, put)
import Effectful.TH
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
import Nostr.InboxModel (InboxModel, awaitAtLeastOneConnected, startInboxModel, stopInboxModel, subscribeToCommentsFor, unsubscribeToCommentsFor)
import Nostr.Keys (PubKeyXO, derivePublicKeyXO, keyPairToPubKeyXO, pubKeyXOToHex, secKeyToKeyPair)
import Nostr.Publisher
import Nostr.Relay (RelayURI, getUri, isInboxCapable, isValidRelayURI)
import Nostr.RelayConnection (RelayConnection, connect, disconnect)
import Nostr.Subscription
import Nostr.SubscriptionHandler
import Nostr.Util
import Presentation.KeyMgmtUI (KeyMgmtUI)
import Presentation.RelayMgmtUI (RelayMgmtUI)
import RelayMgmt (RelayMgmt)
import Store.Lmdb ( LmdbState(..), LmdbStore, initialLmdbState, initializeLmdbState
                  , getEvent, getFollows, getGeneralRelays )
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
  = ProfileResult { npub :: Text, relayUri :: Maybe Text }
  | NoResult
  deriving (Eq, Generic, Show)

instance ToJSON SearchResult where
  toEncoding (ProfileResult npub' relayUri') = pairs
     ( "npub"  .= npub'
    <> "relayUri" .= relayUri'
     )
  toEncoding NoResult = pairs ( "result" .= ("no_result" :: Text) )


-- | Futr Effects.
data Futr :: Effect where
  Login :: ObjRef () -> Text -> Futr m ()
  Search :: ObjRef () -> Text -> Futr m SearchResult
  SetCurrentProfile :: Text -> Futr m ()
  SetCurrentPost :: Maybe EventId -> Futr m ()
  FollowProfile :: Text -> Futr m ()
  UnfollowProfile :: Text -> Futr m ()
  OpenChat :: PubKeyXO -> Futr m ()
  SendPrivateMessage :: Text -> Futr m ()
  SendShortTextNote :: Text -> Futr m ()
  Logout :: ObjRef () -> Futr m ()
  Repost :: EventId -> Futr m ()
  QuoteRepost :: EventId -> Text -> Futr m ()
  Comment :: EventId -> Text -> Futr m ()
  DeleteEvent :: EventId -> Text -> Futr m ()


-- | Dispatch type for Futr effect.
type instance DispatchOf Futr = Dynamic


makeEffect ''Futr


-- | Effectful type for Futr.
type FutrEff es =
  ( State AppState :> es
  , State LmdbState :> es
  , LmdbStore :> es
  , KeyMgmt :> es
  , KeyMgmtUI :> es
  , RelayMgmtUI :> es
  , Nostr :> es
  , InboxModel :> es
  , RelayConnection :> es
  , RelayMgmt :> es
  , Subscription :> es
  , Publisher :> es
  , State KeyMgmtState :> es
  , State RelayPool :> es
  , State QtQuickState :> es
  , QtQuick :> es
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
          logInfo $ "Starting login for account: " <> pack (show $ accountPubKeyXO a)
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

    if not ("nprofile" `isPrefixOf` input || "npub" `isPrefixOf` input)
      then return NoResult
      else case parseNprofileOrNpub input of
        Nothing -> return NoResult
        Just (pubkey', maybeRelay) 
          | Just pubkey' == myPubKey -> return NoResult
          | otherwise -> do
              currentFollows <- maybe [] id <$> traverse getFollows myPubKey
              if any (\f -> pubkey f == pubkey') currentFollows
                then return $ ProfileResult (pubKeyXOToBech32 pubkey') maybeRelay
                else do
                  searchInRelays pubkey' maybeRelay
                  return $ ProfileResult (pubKeyXOToBech32 pubkey') maybeRelay

  SetCurrentProfile npub' -> do
    case bech32ToPubKeyXO npub' of
      Just pk -> do
        modify @AppState $ \st -> st { currentProfile = Just pk }
        notify $ emptyUpdates { profilesChanged = True }
      Nothing -> do
        logError $ "Invalid npub, cannot set current profile: " <> npub'
        return ()

  SetCurrentPost eid -> do
    previousPost <- gets @AppState currentPost

    modify @AppState $ \st -> st { currentPost = eid }

    case (eid, previousPost) of
      (Just newId, _) -> subscribeToCommentsFor newId
      (Nothing, Just oldId) -> do
        clearCommentsRef
        unsubscribeToCommentsFor oldId
      (Nothing, Nothing) -> clearCommentsRef
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

  OpenChat pubKeyXO -> do
    st <- get @AppState

    case currentContact st of
      (Just _, Just subId') -> stopSubscription subId'
      _ -> return ()

    modify $ \st' -> st' { currentContact = (Just pubKeyXO, Nothing) }
    notify $ emptyUpdates { privateMessagesChanged = True }

  SendPrivateMessage input -> do
    st <- get @AppState
    case (keyPair st, currentContact st) of
      (Just kp, (Just recipient, _)) -> do
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
      (_, (Nothing, _)) -> logError "No current chat recipient"

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
    -- Close current Lmdb environment if it exists
    st <- get @LmdbState
    liftIO $ closeEnvironment (lmdbEnv st)

    -- Reset LMDB state to initial
    put @LmdbState initialLmdbState

    -- Reset application state
    modify @AppState $ \st' -> st'
        { keyPair = Nothing
        , currentScreen = KeyMgmt
        }

    stopInboxModel
    -- Wait a moment for disconnects to process
    threadDelay 100000  -- 100ms delay

    modify @RelayPool $ const initialRelayPool

    fireSignal obj
    logInfo "User logged out successfully"

  Repost eid -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- getEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just EventWithRelays{event, relays} -> do
            let e = createRepost event (Set.findMin relays) (keyPairToPubKeyXO kp) now
            signed <- signEvent e kp
            case signed of
              Just s -> do
                publishToOutbox s
                mEventAndRelays <- getEvent eid
                case mEventAndRelays of
                  Just EventWithRelays{event = origEvent, relays = relaySet} -> do
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
              Nothing -> logError "Failed to sign repost"

  QuoteRepost eid quote -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- getEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just EventWithRelays{event, relays} -> do
            let q = createQuoteRepost event (Set.findMin relays) quote (keyPairToPubKeyXO kp) now
            signed <- signEvent q kp
            case signed of
              Just s -> do
                publishToOutbox s
                notify $ emptyUpdates { postsChanged = True }
              Nothing -> logError "Failed to sign quote repost"
    where
      createQuoteRepost :: Event -> RelayURI -> Text -> PubKeyXO -> Int -> UnsignedEvent
      createQuoteRepost event relayUrl quote' xo t =
        UnsignedEvent
          { pubKey' = xo
          , createdAt' = t
          , kind' = NE.ShortTextNote
          , tags' = [ ["q", eventIdToHex $ eventId event, relayUrl, pubKeyXOToHex $ pubKey event]
                    ]
          , content' = quote' <> "\n\nnostr:" <> eventToNevent event (Just relayUrl)
          }

  Comment eid comment' -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- getEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just ev -> do
            let c = createComment (event ev) comment' (keyPairToPubKeyXO kp) now
            signed <- signEvent c kp
            case signed of
              Just s -> do
                publishToOutbox s
                mEventAndRelays <- getEvent eid
                case mEventAndRelays of
                  Just EventWithRelays{event = origEvent, relays = relaySet} -> do
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
              Nothing -> logError "Failed to sign comment"

  Futr.DeleteEvent eid reason -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        let deletion = createEventDeletion [eid] reason (keyPairToPubKeyXO kp) now
        signed <- signEvent deletion kp
        case signed of
          Just s -> do
            publishToOutbox s
            notify $ emptyUpdates { postsChanged = True, privateMessagesChanged = True }
          Nothing -> logError "Failed to sign event deletion"


-- Helper function to parse nprofile or npub
parseNprofileOrNpub :: Text -> Maybe (PubKeyXO, Maybe RelayURI)
parseNprofileOrNpub input = 
  case bech32ToPubKeyXO input of
    Just pubkey' -> Just (pubkey', Nothing)  -- for npub
    Nothing -> case nprofileToPubKeyXO input of
      Just (pubkey', relays') -> Just (pubkey', listToMaybe relays')  -- for nprofile
      Nothing -> Nothing


-- | Login with an account.
loginWithAccount :: FutrEff es => ObjRef () -> Account -> Eff es ()
loginWithAccount obj a = do
    modify @AppState $ \s -> s { keyPair = Just (secKeyToKeyPair $ accountSecKey a) }
    modify @KeyMgmtState $ \st -> st
        { nsecView = secKeyToBech32 $ accountSecKey a
        , npubView = pubKeyXOToBech32 $ derivePublicKeyXO $ accountSecKey a
        }

    void $ async $ startInboxModel
    atLeastOneConnected <- awaitAtLeastOneConnected

    if atLeastOneConnected
      then do
        modify @AppState $ \s -> s { currentScreen = Home }
        liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj True ""
        fireSignal obj
      else do
        liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Failed to connect to any relay"


-- | Send a follow list event.
sendFollowListEvent :: FutrEff es => [Follow] -> Eff es ()
sendFollowListEvent follows = do
    st <- get @AppState
    case keyPair st of
        Nothing -> logError "No keypair found"
        Just kp -> do
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
searchInRelays :: FutrEff es => PubKeyXO -> Maybe RelayURI -> Eff es ()
searchInRelays xo mr = do
    manuallyConnected <- case mr of
        Just relayUri -> do
            conns <- gets @RelayPool activeConnections
            if Map.member relayUri conns
                then return False
                else do
                    void $ connect relayUri
                    return True
        Nothing -> return False

    relays <- getGeneralRelays xo
    conns <- gets @RelayPool activeConnections

    let searchRelays = case mr of
            Just uri -> uri : map getUri relays
            Nothing -> map getUri relays

    forM_ searchRelays $ \relayUri' -> do
        when (Map.member relayUri' conns) $ do
            q <- newTQueueIO
            subId' <- subscribe relayUri' (metadataFilter [xo]) q
            -- @todo duplicated from subscription handler, but closes unneeded connections
            
            void $ async $ do
                shouldStopVar <- newTVarIO False
                let loop = do
                        e <- atomically $ readTQueue q
                        es <- atomically $ flushTQueue q
                    
                        forM_ (e:es) $ \(relayUri, e') -> do
                            case e' of
                                EventAppeared event' -> handleEvent relayUri event'

                                SubscriptionEose _ -> do
                                  stopSubscription subId'
                                  when (manuallyConnected && Just relayUri' == mr) $ do
                                    disconnect relayUri'

                                SubscriptionClosed _ -> do
                                  atomically $ writeTVar shouldStopVar True
                                  when (manuallyConnected && Just relayUri' == mr) $ do
                                    disconnect relayUri'

                        shouldStop <- atomically $ readTVar shouldStopVar
                        unless shouldStop loop
                
                loop
