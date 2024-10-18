{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Futr where

import Control.Monad (forM, forM_, void, unless, when)
import Data.Aeson (ToJSON, pairs, toEncoding, (.=))
import Data.Maybe (listToMaybe)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text, isPrefixOf)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (Async, async, waitAny)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import EffectfulQML
import GHC.Generics (Generic)
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Graphics.QML qualified as QML

import Logging
import Nostr.Bech32
import Nostr.Event (createFollowList, signEvent)
import Nostr.Keys (KeyPair, PubKeyXO, keyPairToPubKeyXO, secKeyToKeyPair)
import Nostr.GiftWrap
import Nostr.RelayPool
import Nostr.Subscription
import Nostr.Types ( Event, RelayURI, Relay(..), UnsignedEvent(..),
                     followListFilter, giftWrapFilter, metadataFilter,
                     relayName, relayURIToText)
import Nostr.Util
import Presentation.KeyMgmt qualified as PKeyMgmt
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
  Login :: ObjRef () -> Text -> Futr m Bool
  Search :: ObjRef () -> Text -> Futr m SearchResult
  SetCurrentProfile :: Text -> Futr m ()
  FollowProfile :: Text -> Futr m ()
  UnfollowProfile :: Text -> Futr m ()
  OpenChat :: PubKeyXO -> Futr m ()
  Logout :: ObjRef () -> Futr m ()


-- | Dispatch type for Futr effect.
type instance DispatchOf Futr = Dynamic


makeEffect ''Futr


-- | Effectful type for Futr.
type FutrEff es = ( State AppState :> es
                  , PKeyMgmt.KeyMgmt :> es
                  , PKeyMgmt.KeyMgmtUI :> es
                  , RelayPool :> es
                  , Subscription :> es
                  , State PKeyMgmt.KeyMgmtState :> es
                  , State RelayPoolState :> es
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
      kst <- get @PKeyMgmt.KeyMgmtState
      case Map.lookup (PKeyMgmt.AccountId input) (PKeyMgmt.accountMap kst) of
        Just a -> do
          success <- loginWithAccount obj a
          return success
        Nothing -> return False

  Search _ input -> do
    st <- get @AppState
    let myPubKey = keyPairToPubKeyXO <$> keyPair st

    case input of
      _ | "nprofile" `isPrefixOf` input || "npub" `isPrefixOf` input -> do
        case parseNprofileOrNpub input of
          Just (pubkey', maybeRelay) -> do
            case myPubKey of
              Just myKey | myKey == pubkey' -> do
                return $ ProfileResult (pubKeyXOToBech32 pubkey') (relayURIToText <$> maybeRelay)

              _ -> do
                let userFollows = maybe [] (flip (Map.findWithDefault []) (followList $ follows st)) myPubKey
                if any (\(Follow pk _ _) -> pk == pubkey') userFollows
                  then do
                    return $ ProfileResult (pubKeyXOToBech32 pubkey') (relayURIToText <$> maybeRelay)
                  else do
                    case maybeRelay of
                      Just relay' -> return $ ProfileResult (pubKeyXOToBech32 pubkey') (Just $ relayURIToText relay')
                      Nothing -> do
                        relays' <- gets @RelayPoolState relays
                        let relaysToSearch = Map.keys relays'
                        forM_ relaysToSearch $ \relay' -> do
                          void $ async $ do
                            maybeSubInfo <- startSubscription relay' [metadataFilter [pubkey']]
                            case maybeSubInfo of
                              Just (subId, queue) -> do
                                handleResponsesUntilEOSE relay' queue
                                stopSubscription subId
                              Nothing -> 
                                logWarning $ "Failed to start search subscription for relay: " <> relayURIToText relay'

                        return $ ProfileResult (pubKeyXOToBech32 pubkey') Nothing

          Nothing -> return NoResult

      _ -> return NoResult

  SetCurrentProfile npub' -> do
    case bech32ToPubKeyXO npub' of
      Just pk -> do
        modify @AppState $ \st -> st { currentProfile = Just pk }
        obj <- gets @AppState profileObjRef
        case obj of
          Just obj' -> fireSignal obj'
          Nothing -> return ()
      Nothing -> do
        logError $ "Invalid npub, cannot set current profile: " <> npub'
        return ()

  FollowProfile npub' -> do
    let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    case keyPairToPubKeyXO <$> keyPair st of
        Just userPK -> do
            let currentFollows = Map.findWithDefault [] userPK (followList $ follows st)
            unless (any (\follow -> pubkey follow == pubKeyXO) currentFollows) $ do
                let newFollow = Follow pubKeyXO Nothing Nothing
                let newFollows = newFollow : currentFollows
                modify $ \st' -> st' { follows = (follows st') { followList = Map.insert userPK newFollows (followList $ follows st') } }
            notifyUI
            sendFollowListEvent
        Nothing -> return ()

  UnfollowProfile npub' -> do
    let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    let userPubKey = keyPairToPubKeyXO <$> keyPair st
    case userPubKey of
        Just userPK -> do
            let currentFollows = Map.findWithDefault [] userPK (followList $ follows st)
            let newFollows = filter (\follow -> pubkey follow /= pubKeyXO) currentFollows
            modify $ \st' -> st' { follows = (follows st') { followList = Map.insert userPK newFollows (followList $ follows st') } }
            notifyUI
            sendFollowListEvent
        Nothing -> return ()

  OpenChat pubKeyXO -> do
    st <- get @AppState

    case currentChatRecipient st of
      (Just _, Just subId') -> stopSubscription subId'
      _ -> return ()

    modify $ \st' -> st' { currentChatRecipient = (Just [pubKeyXO], Nothing) }
    notifyUI

  Logout obj -> do
      modify @AppState $ \st -> st
        { keyPair = Nothing
        , currentScreen = KeyMgmt
        , follows = FollowModel Map.empty (objRef $ follows st)
        , profiles = Map.empty
        , confirmations = Map.empty
        }

      relays' <- gets @RelayPoolState relays
      mapM_ disconnect (Map.keys relays')

      modify @RelayPoolState $ \st -> st
        { relays = Map.empty
        }

      fireSignal obj
      logInfo "User logged out successfully"


-- Helper function to parse nprofile or npub
parseNprofileOrNpub :: Text -> Maybe (PubKeyXO, Maybe RelayURI)
parseNprofileOrNpub input = 
  case bech32ToPubKeyXO input of
    Just pubkey' -> Just (pubkey', Nothing)  -- For npub
    Nothing -> case nprofileToPubKeyXO input of
      Just (pubkey', relays') -> Just (pubkey', listToMaybe relays')  -- For nprofile
      Nothing -> Nothing


-- | Login with an account.
loginWithAccount :: FutrEff es => ObjRef () -> PKeyMgmt.Account -> Eff es Bool
loginWithAccount obj a = do
  let kp = secKeyToKeyPair $ PKeyMgmt.nsec a
  let xo = keyPairToPubKeyXO kp 
  let rs = PKeyMgmt.relays a

  -- add all relays to the relay pool
  mapM_ addRelay rs

  -- For each relay, asynchronously connect and handle subscriptions
  connectionResults <- forM rs $ \relay' -> async $ do
    isConnected <- connect relay'

    if isConnected
      then do
        logDebug $ "Connected to relay: " <> relayName relay'
        modify @AppState $ \st -> st { keyPair = Just kp, currentScreen = Home }
        fireSignal obj
        liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj True ""
        
        -- Initial subscription (until EOSE)
        let initialFilters =
              [ followListFilter [ xo ]
              , metadataFilter [ xo ]
              ]

        logDebug $ "Starting initial subscription for relay: " <> relayName relay'

        maybeSubInfo <- startSubscription (uri relay') initialFilters
        case maybeSubInfo of
          Nothing -> logWarning $ "Failed to start initial subscription for relay: " <> relayName relay'
          Just (subId', queue) -> do
            void $ async $ do
              handleResponsesUntilEOSE (uri relay') queue
              stopSubscription subId'
              fireSignal obj

              -- Start the main subscription after EOSE
              st <- get @AppState
              let followedPubKeys = concatMap (\(_, follows') -> map (\(Follow pk _ _) -> pk) follows') $ Map.toList $ followList $ follows st
              let filters =
                    [ followListFilter (xo : followedPubKeys)
                    , metadataFilter (xo : followedPubKeys)
                    , giftWrapFilter xo
                    ]

              maybeSubInfo' <- startSubscription (uri relay') filters
              case maybeSubInfo' of
                Nothing -> logWarning $ "Failed to start main subscription for relay: " <> (relayURIToText $ uri relay')
                Just (_, queue') -> do
                  handleResponsesUntilClosed (uri relay') queue'
        return True
      else return False

  atLeastOneConnected <- waitForFirstTrueOrAllFalse connectionResults

  when (not atLeastOneConnected) $ do
    liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Failed to connect to any relay"

  return atLeastOneConnected
  where
    waitForFirstTrueOrAllFalse :: FutrEff es => [Async Bool] -> Eff es Bool
    waitForFirstTrueOrAllFalse [] = return False
    waitForFirstTrueOrAllFalse asyncs = do
      (completed, result) <- waitAny asyncs
      if result
        then return True
        else do
          let remainingAsyncs = filter (/= completed) asyncs
          waitForFirstTrueOrAllFalse remainingAsyncs

{-
-- | Handle the search subscription, updating profiles and stopping on EOSE.
handleSearchSubscription :: FutrEff es => RelayURI -> TQueue Response -> Eff es ()
handleSearchSubscription relayURI' queue = do
  msg <- atomically $ readTQueue queue
  msgs <- atomically $ flushTQueue queue
  void $ processResponses relayURI' (msg : msgs)
  notifyUI
-}

-- | Send a follow list event.
sendFollowListEvent :: FutrEff es => Eff es ()
sendFollowListEvent = do
    st <- get @AppState
    case keyPair st of
        Just kp -> do
          currentTime <- getCurrentTime
          let userPK = keyPairToPubKeyXO kp
          let followList' = Map.findWithDefault [] userPK (followList $ follows st)
          let followTuples = map (\(Follow pk _ petName') -> (pk, petName')) followList'
          let event = createFollowList followTuples userPK currentTime
          signedEvent <- signEvent' event kp
          case signedEvent of
            Just signedEvent' -> do
              relays' <- gets @RelayPoolState relays
              sendEvent signedEvent' (Map.keys relays')
            Nothing -> do
              logError "Failed to sign follow list event"
              return ()
        Nothing -> return ()


-- | Sign an event.
signEvent' :: FutrEff es => UnsignedEvent -> KeyPair -> Eff es (Maybe Event)
signEvent' e kp = liftIO $ signEvent e kp
