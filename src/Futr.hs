{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Futr where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (ToJSON, pairs, toEncoding, (.=))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text, isPrefixOf)
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
import Nostr.Event (createFollowList, createRumor)
import Nostr.Keys (PubKeyXO, derivePublicKeyXO, keyPairToPubKeyXO, secKeyToKeyPair)
import Nostr.GiftWrap
import Nostr.Publisher
import Nostr.RelayPool
import Nostr.Subscription
import Nostr.Types ( Relay(..), RelayURI, Tag(..)
                   , getUri, metadataFilter )
import Nostr.Util
import Presentation.KeyMgmtUI (KeyMgmtUI)
import Presentation.RelayMgmtUI (RelayMgmtUI)
import Types

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
  Search :: ObjRef () -> Text -> Futr m ()
  SetCurrentProfile :: Text -> Futr m ()
  FollowProfile :: Text -> Futr m ()
  UnfollowProfile :: Text -> Futr m ()
  OpenChat :: PubKeyXO -> Futr m ()
  SendMessage :: Text -> Futr m ()
  Logout :: ObjRef () -> Futr m ()


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
      then return ()
      else case parseNprofileOrNpub input of
        Nothing -> return ()
        Just (pubkey', maybeRelay) 
          | Just pubkey' == myPubKey -> 
              return ()
          | otherwise -> do
              let userFollows = maybe [] (flip (Map.findWithDefault []) (follows st)) myPubKey
              if any (\(Follow pk _ _) -> pk == pubkey') userFollows
                then return ()
                else searchInRelays pubkey' maybeRelay

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

    case currentChatRecipient st of
      (Just _, Just subId') -> stopSubscription subId'
      _ -> return ()

    modify $ \st' -> st' { currentChatRecipient = (Just [pubKeyXO], Nothing) }
    notify $ emptyUpdates { chatsChanged = True }

  SendMessage input -> do
    st <- get @AppState
    case (keyPair st, currentChatRecipient st) of
      (Just kp, (Just recipients, _)) -> do
        now <- getCurrentTime
        let senderPubKeyXO = keyPairToPubKeyXO kp
            allRecipients = senderPubKeyXO : recipients
            rumor = createRumor senderPubKeyXO now (map (\xo -> PTag xo Nothing Nothing) recipients) input

        giftWraps <- forM allRecipients $ \recipient -> do
          seal <- createSeal rumor kp recipient
          case seal of
            Just seal' -> do
              giftWrapResult <- createGiftWrap seal' recipient
              case giftWrapResult of
                Just (gw, _) -> return (Just gw)
                Nothing -> logError "Failed to create gift wrap" >> return Nothing
            Nothing -> logError "Failed to create seal" >> return Nothing

        let validGiftWraps = catMaybes giftWraps
        forM_ validGiftWraps $ \gw -> publishGiftWrap gw senderPubKeyXO

      (Nothing, _) -> logError "No key pair found"
      (_, (Nothing, _)) -> logError "No current chat recipient"

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
    forM_ relays $ \r -> do
      when (isInboxCapable r) $ do
        let relayUri' = getUri r
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
