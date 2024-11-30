{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Futr where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (ToJSON, pairs, toEncoding, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Set qualified as Set
import Data.Text (Text, isPrefixOf, pack)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically, readTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import EffectfulQML
import GHC.Generics (Generic)
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Graphics.QML qualified as QML

import Logging
import KeyMgmt (Account(..), AccountId(..), KeyMgmt, KeyMgmtState(..))
import Nostr
import Nostr.Bech32
import Nostr.Event (createComment, createFollowList, createQuoteRepost, createRepost, createRumor, createShortTextNote)  
import Nostr.Keys (PubKeyXO, derivePublicKeyXO, keyPairToPubKeyXO, secKeyToKeyPair)
import Nostr.GiftWrap
import Nostr.Publisher
import Nostr.RelayPool
import Nostr.Subscription
import Nostr.Types ( Event(..), EventId, Profile(..), Relay(..), RelayURI, Tag(..)
                   , getUri, metadataFilter )
import Nostr.Util
import Presentation.KeyMgmtUI (KeyMgmtUI)
import Presentation.RelayMgmtUI (RelayMgmtUI)
import Types hiding (Comment, QuoteRepost, Repost)

-- | Signal key class for LoginStatusChanged.
data LoginStatusChanged deriving Typeable

instance SignalKeyClass LoginStatusChanged where
    type SignalParams LoginStatusChanged = Bool -> Text -> IO ()


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
  FollowProfile :: Text -> Futr m ()
  UnfollowProfile :: Text -> Futr m ()
  OpenChat :: PubKeyXO -> Futr m ()
  SendMessage :: Text -> Futr m ()
  SendShortTextNote :: Text -> Futr m ()
  Logout :: ObjRef () -> Futr m ()
  Repost :: EventId -> Futr m ()
  QuoteRepost :: EventId -> Text -> Futr m ()
  Comment :: EventId -> Text -> Futr m ()


-- | Dispatch type for Futr effect.
type instance DispatchOf Futr = Dynamic


makeEffect ''Futr


-- | Effectful type for Futr.
type FutrEff es = ( State AppState :> es
                  , KeyMgmt :> es
                  , KeyMgmtUI :> es
                  , RelayMgmtUI :> es
                  , Nostr :> es
                  , RelayPool :> es
                  , Subscription :> es
                  , Publisher :> es
                  , State KeyMgmtState :> es
                  , State RelayPoolState :> es
                  , State EffectfulQMLState :> es
                  , GiftWrap :> es
                  , EffectfulQML :> es
                  , Logging :> es
                  , IOE :> es
                  , Concurrent :> es
                  , Util :> es
                  )


-- | Run the Futr effect.
runFutr :: FutrEff es => Eff (Futr : es) a -> Eff es a
runFutr = interpret $ \_ -> \case
  Login obj input -> do
      kst <- get @KeyMgmtState
      case Map.lookup (AccountId input) (accountMap kst) of
        Just a -> loginWithAccount obj a
        Nothing -> liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Account not found"

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
              let userFollows = maybe [] (flip (Map.findWithDefault []) (follows st)) myPubKey
              if any (\(Follow pk _ _) -> pk == pubkey') userFollows
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

  FollowProfile npub' -> do
    let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    case keyPairToPubKeyXO <$> keyPair st of
        Just userPK -> do
            let currentFollows = Map.findWithDefault [] userPK (follows st)
            unless (any (\follow -> pubkey follow == pubKeyXO) currentFollows) $ do
                let newFollow = Follow pubKeyXO Nothing Nothing
                let newFollows = newFollow : currentFollows
                modify $ \st' -> st' { follows = Map.insert userPK newFollows (follows st') }
                notify $ emptyUpdates { followsChanged = True }
            sendFollowListEvent
        Nothing -> return ()

  UnfollowProfile npub' -> do
    let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    let userPubKey = keyPairToPubKeyXO <$> keyPair st
    case userPubKey of
        Just userPK -> do
            let currentFollows = Map.findWithDefault [] userPK (follows st)
            let newFollows = filter (\follow -> pubkey follow /= pubKeyXO) currentFollows
            modify $ \st' -> st' { follows = Map.insert userPK newFollows (follows st') }
            notify $ emptyUpdates { followsChanged = True }
            sendFollowListEvent
        Nothing -> return ()

  OpenChat pubKeyXO -> do
    st <- get @AppState

    case currentContact st of
      (Just _, Just subId') -> stopSubscription subId'
      _ -> return ()

    modify $ \st' -> st' { currentContact = (Just pubKeyXO, Nothing) }
    notify $ emptyUpdates { privateMessagesChanged = True }

  SendMessage input -> do
    st <- get @AppState
    case (keyPair st, currentContact st) of
      (Just kp, (Just recipient, _)) -> do
        now <- getCurrentTime
        let senderPubKeyXO = keyPairToPubKeyXO kp
            allRecipients = senderPubKeyXO : recipient : []
            rumor = createRumor senderPubKeyXO now (map (\xo -> PTag xo Nothing Nothing) [recipient]) input

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
        forM_ validGiftWraps $ \gw -> publishGiftWrap gw senderPubKeyXO

      (Nothing, _) -> logError "No key pair found"
      (_, (Nothing, _)) -> logError "No current chat recipient"

  SendShortTextNote input -> do
    kp <- getKeyPair
    now <- getCurrentTime
    let u = createShortTextNote input (keyPairToPubKeyXO kp) now
    signed <- signEvent u kp
    case signed of
      Just s -> publishToOutbox s
      Nothing -> logError "Failed to sign short text note"

  Logout obj -> do
      modify @AppState $ \st -> st
        { keyPair = Nothing
        , currentScreen = KeyMgmt
        , follows = Map.empty
        , profiles = Map.empty
        }

      conns <- gets @RelayPoolState activeConnections
      mapM_ disconnect (Map.keys conns)

      -- Wait a moment for disconnects to process
      threadDelay 100000  -- 100ms delay

      modify @RelayPoolState $ const initialRelayPoolState

      fireSignal obj
      logInfo "User logged out successfully"

  Repost eid -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- fetchEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just (_, []) -> do
            logError "Failed to fetch event: no relays"
          Just (event, r:_) -> do
            let e = createRepost event r (keyPairToPubKeyXO kp) now
            signed <- signEvent e kp
            case signed of
              Just s -> do
                publishToOutbox s
                case Map.lookup eid (events st) of
                  Just (origEvent, relays) -> do
                    genRelays <- gets @RelayPoolState generalRelays
                    let outboxUris = Set.fromList $ map getUri $ 
                          filter isOutboxCapable $ concatMap fst $ Map.elems genRelays

                    let eventRelayUris = Set.fromList relays
                    rps <- gets @RelayPoolState generalRelays
                    let authorInboxUris = case Map.lookup (pubKey origEvent) rps of
                          Just (authorRelays, _) -> 
                            Set.fromList $ map getUri $ filter isInboxCapable authorRelays
                          Nothing -> Set.empty

                    let targetUris = (eventRelayUris `Set.union` authorInboxUris) 
                                    `Set.difference` outboxUris

                    forM_ (Set.toList targetUris) $ \relay ->
                      publishToRelay s relay
                  Nothing -> return ()
              Nothing -> logError "Failed to sign repost"

  QuoteRepost eid quote -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- fetchEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just (_, []) -> do
            logError "Failed to fetch event: no relays"
          Just (event, r:_) -> do
            let q = createQuoteRepost event r quote (keyPairToPubKeyXO kp) now
            signed <- signEvent q kp
            case signed of
              Just s -> publishToOutbox s
              Nothing -> logError "Failed to sign quote repost"

  Comment eid comment -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- fetchEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just (event, _) -> do
            let c = createComment event comment (Right eid) Nothing Nothing (keyPairToPubKeyXO kp) now
            signed <- signEvent c kp
            case signed of
              Just s -> do
                publishToOutbox s
                -- Publish to relays where the original event was seen
                case Map.lookup eid (events st) of
                  Just (origEvent, relays) -> do
                    -- Publish to all relays where we saw the original event
                    forM_ relays $ \relay -> publishToRelay s relay
                    -- Also publish to inbox relays of the original author
                    let authorPk = pubKey origEvent
                    rps <- gets @RelayPoolState generalRelays
                    case Map.lookup authorPk rps of
                      Just (authorRelays, _) -> 
                        forM_ (filter isInboxCapable authorRelays) $ \relay ->
                          publishToRelay s (getUri relay)
                      Nothing -> return ()
                  Nothing -> return ()
              Nothing -> logError "Failed to sign comment"


-- Helper function to fetch an event
fetchEvent :: EventId -> FutrEff es => Eff es (Maybe (Event, [RelayURI]))
fetchEvent eid = do
  es <- gets @AppState events
  return $ Map.lookup eid es

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
    let (rs, t) = accountRelays a

    modify @AppState $ \s -> s { keyPair = Just (secKeyToKeyPair $ accountSecKey a) }

    modify @KeyMgmtState $ \st -> st
        { nsecView = secKeyToBech32 $ accountSecKey a
        , npubView = pubKeyXOToBech32 $ derivePublicKeyXO $ accountSecKey a
        }

    importGeneralRelays (accountPubKeyXO a) rs t

    forM_ rs $ \relay' -> void $ async $ connect $ getUri relay'

    void $ async $ do
        atLeastOneConnected <- awaitAtLeastOneConnected
        -- Update UI state after connections are established
        when atLeastOneConnected $ do
            modify @AppState $ \s -> s { currentScreen = Home }
            fireSignal obj

        -- Fire final status
        if not atLeastOneConnected 
            then liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Failed to connect to any relay"
            else liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj True ""


-- | Send a follow list event.
sendFollowListEvent :: FutrEff es => Eff es ()
sendFollowListEvent = do
  st <- get @AppState
  case keyPair st of
    Nothing -> logError "No keypair found"
    Just kp -> do
      currentTime <- getCurrentTime
      let userPK = keyPairToPubKeyXO kp
          followList' = Map.findWithDefault [] userPK (follows st)
          followTuples = map (\(Follow pk _ petName') -> (pk, petName')) followList'
          event = createFollowList followTuples userPK currentTime
      signedEvent <- signEvent event kp
      case signedEvent of
        Just signedEvent' -> publishToOutbox signedEvent'
        Nothing -> do
          logError "Failed to sign follow list event"
          return ()


-- | Search for a profile in relays.
searchInRelays :: FutrEff es => PubKeyXO -> Maybe RelayURI -> Eff es ()
searchInRelays pubkey' _ = do
    -- @todo use relay hint
    st <- get @RelayPoolState
    let relays = case Map.lookup pubkey' (generalRelays st) of
                   Just (rs, _) -> rs
                   Nothing -> []
    conns <- gets @RelayPoolState activeConnections
    forM_ relays $ \relay -> do
      when (isInboxCapable relay) $ do
        let relayUri' = getUri relay
        when (Map.member relayUri' conns) $ do
          subId' <- newSubscriptionId
          mq <- subscribe relayUri' subId' [metadataFilter [pubkey']]
          case mq of
              Nothing -> return ()
              Just q -> void $ async $ do
                  let loop = do
                        e <- atomically $ readTQueue q
                        case e of
                          EventAppeared event' -> do
                            updates <- handleEvent relayUri' subId' [metadataFilter [pubkey']] event'
                            notify updates
                            loop
                          SubscriptionEose -> do
                            stopSubscription subId'
                            loop
                          SubscriptionClosed _ -> return () -- stop the loop
                  loop


isInboxCapable :: Relay -> Bool
isInboxCapable (InboxRelay _) = True
isInboxCapable (InboxOutboxRelay _) = True
isInboxCapable _ = False


-- | Get the content of a post.
getPostContent :: FutrEff es => Post -> Eff es (Maybe Text)
getPostContent post = do
    st <- get @AppState 
    return $ Map.lookup (postId post) (events st) >>= Just . content . fst

-- | Get the creation timestamp of a post.
getPostCreatedAt :: FutrEff es => Post -> Eff es (Maybe Int)
getPostCreatedAt post = do
    st <- get @AppState
    return $ Map.lookup (postId post) (events st) >>= Just . createdAt . fst


-- | Get the content of a referenced post.
getReferencedContent :: FutrEff es => EventId -> Eff es (Maybe Text)
getReferencedContent eid = do
    st <- get @AppState
    return $ Map.lookup eid (events st) >>= Just . content . fst

-- | Get the author of a referenced post.
getReferencedAuthor :: FutrEff es => EventId -> Eff es (Maybe PubKeyXO)
getReferencedAuthor eid = do
    st <- get @AppState
    return $ Map.lookup eid (events st) >>= Just . pubKey . fst


-- | Get the author name of a referenced post.
getReferencedAuthorName :: FutrEff es => EventId -> Eff es (Maybe Text)
getReferencedAuthorName eid = do
    st <- get @AppState
    case Map.lookup eid (events st) of
        Nothing -> return Nothing
        Just (event, _) -> do
            let authorPubKey = pubKey event
                authorProfile = Map.lookup authorPubKey (profiles st)
            return $ case authorProfile of
                Just profileData -> Just $ fromMaybe 
                    (fromMaybe (pubKeyXOToBech32 authorPubKey) (name $ fst profileData))
                    (displayName $ fst profileData)
                Nothing -> Just $ pubKeyXOToBech32 authorPubKey

-- | Get the author picture of a referenced post.
getReferencedAuthorPicture :: FutrEff es => EventId -> Eff es (Maybe Text)
getReferencedAuthorPicture eid = do
    st <- get @AppState
    case Map.lookup eid (events st) of
        Nothing -> return Nothing
        Just (event, _) -> do
            let authorPubKey = pubKey event
            return $ Map.lookup authorPubKey (profiles st) >>= picture . fst

-- | Get the creation timestamp of a referenced post.
getReferencedCreatedAt :: FutrEff es => EventId -> Eff es (Maybe Int)
getReferencedCreatedAt eid = do
    st <- get @AppState
    return $ Map.lookup eid (events st) >>= Just . createdAt . fst
