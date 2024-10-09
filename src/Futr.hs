{-# LANGUAGE BlockArguments #-}

module Futr where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Control.Monad (forM, void, unless, when)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import Data.Typeable (Typeable)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (Async, async, waitAny)
import Effectful.Concurrent.STM (TQueue, atomically, readTQueue, flushTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Graphics.QML qualified as QML

import AppState
import Nostr.Effects.CurrentTime
import Nostr.Effects.Logging
import Nostr.Effects.RelayPool
import Nostr.Keys (keyPairToPubKeyXO, secKeyToKeyPair)
import Nostr.Types ( Event(..), EventId(..), Filter(..), Kind(..), Tag(..), 
                     RelayURI, Response(..), Relay(..), relayName, relayURIToText)
import Presentation.KeyMgmt qualified as PKeyMgmt



  -- | Futr Effect for managing the application state.
data Futr :: Effect where
  Login :: ObjRef () -> Text -> Futr m Bool
  Logout :: ObjRef () -> Futr m ()


-- | Dispatch type for Futr effect.
type instance DispatchOf Futr = Dynamic


makeEffect ''Futr


-- | Signal key class for LoginStatusChanged.
data LoginStatusChanged deriving Typeable


instance SignalKeyClass LoginStatusChanged where
    type SignalParams LoginStatusChanged = Bool -> Text -> IO ()


-- | Futr Effect
type FutrEff es = ( State AppState :> es
                  , PKeyMgmt.KeyMgmt :> es
                  , PKeyMgmt.KeyMgmtUI :> es
                  , RelayPool :> es
                  , State PKeyMgmt.KeyMgmtState :> es
                  , State RelayPoolState :> es
                  , EffectfulQML :> es
                  , Logging :> es
                  , IOE :> es
                  , Concurrent :> es
                  , CurrentTime :> es
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
        liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj True ""
        
        -- Initial subscription (until EOSE)
        let initialFilters =
              [ FollowListFilter [ xo ]
              , MetadataFilter [ xo ]
              ]

        logDebug $ "Starting initial subscription for relay: " <> relayName relay'

        maybeSubInfo <- startSubscription (uri relay') initialFilters
        case maybeSubInfo of
          Nothing -> logWarning $ "Failed to start initial subscription for relay: " <> relayName relay'
          Just (subId', queue) -> do
            void $ async $ do
              handleResponsesUntilEOSE (uri relay') queue
              stopSubscription subId'

              -- Start the main subscription after EOSE
              st <- get @AppState
              let followedPubKeys = concatMap (\(_, follows') -> map (\(Follow pk _ _) -> pk) follows') $ Map.toList $ followList $ follows st
              let filters =
                    [ FollowListFilter (xo : followedPubKeys)
                    , MetadataFilter (xo : followedPubKeys)
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


-- | Handle responses until EOSE.
handleResponsesUntilEOSE :: FutrEff es => RelayURI -> TQueue Response -> Eff es ()
handleResponsesUntilEOSE relayURI' queue = do
  msg <- atomically $ readTQueue queue
  msgs <- atomically $ flushTQueue queue
  stopped <- processResponsesUntilEOSE relayURI' (msg : msgs)
  notifyUI
  unless stopped $ handleResponsesUntilEOSE relayURI' queue


-- | Process responses until EOSE.
processResponsesUntilEOSE :: FutrEff es => RelayURI -> [Response] -> Eff es Bool
processResponsesUntilEOSE _ [] = return False
processResponsesUntilEOSE relayURI' (r:rs) = case r of
  EventReceived _ event' -> do
    handleEvent event'
    processResponsesUntilEOSE relayURI' rs
  Eose _ -> return True
  Closed _ _ -> return True
  Ok eventId' accepted' msg -> do
    modify $ handleConfirmation eventId' accepted' msg relayURI'
    processResponsesUntilEOSE relayURI' rs
  Notice msg -> do
    modify $ handleNotice relayURI' msg
    processResponsesUntilEOSE relayURI' rs


-- | Handle responses until closed.
handleResponsesUntilClosed :: FutrEff es => RelayURI -> TQueue Response -> Eff es ()
handleResponsesUntilClosed relayURI' queue = do
  msg <- atomically $ readTQueue queue
  msgs <- atomically $ flushTQueue queue
  stopped <- processResponses relayURI' (msg : msgs)
  notifyUI
  unless stopped $ handleResponsesUntilClosed relayURI' queue


-- | Notify the UI.
notifyUI :: FutrEff es => Eff es ()
notifyUI = do
  st <- get @AppState
  -- @todo: fire signal for other lists as needed
  case objRef $ follows st of
    Just obj' -> fireSignal obj'
    Nothing -> logWarning "No objRef for follows in AppState"


-- | Process responses.
processResponses :: FutrEff es => RelayURI -> [Response] -> Eff es Bool
processResponses _ [] = return False
processResponses relayURI' (r:rs) = case r of
  EventReceived _ event' -> do
    handleEvent event'
    processResponses relayURI' rs
  Eose subId' -> do
    logDebug $ "EOSE on subscription " <> subId'
    processResponses relayURI' rs
  Closed subId msg -> do
    logDebug $ "Closed subscription " <> subId <> " with message " <> msg
    return True
  Ok eventId' accepted' msg -> do
    logDebug $ "OK on subscription " <> pack ( show eventId' ) <> " with message " <> msg
    modify $ handleConfirmation eventId' accepted' msg relayURI'
    processResponses relayURI' rs
  Notice msg -> do
    modify $ handleNotice relayURI' msg
    processResponses relayURI' rs


-- | Handle an event.
handleEvent :: FutrEff es => Event -> Eff es ()
handleEvent event' = case kind event' of
  Metadata -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 $ content event') of
    Right profile -> do
      modify $ \st ->
        st { profiles = Map.insertWith (\new old -> if snd new > snd old then new else old)
                                 (pubKey event')
                                 (profile, createdAt event')
                                 (profiles st)
           }
    Left err -> logWarning $ "Failed to decode metadata: " <> pack err

  FollowList -> do
    let followList' = [Follow pubKey' relayUri' displayName' | PTag pubKey' relayUri' displayName' <- tags event']
    modify $ \st -> st { follows = FollowModel (Map.insert (pubKey event') followList' (followList $ follows st)) (objRef $ follows st) }

  _ -> logDebug $ "Ignoring event of kind: " <> pack (show (kind event'))


-- | Handle a notice.
handleNotice :: RelayURI -> Text -> RelayPoolState -> RelayPoolState
handleNotice relayURI' msg st =
  st { relays = Map.adjust (\rd -> rd { notices = msg : notices rd }) relayURI' (relays st) }


-- | Handle a confirmation.
handleConfirmation :: EventId -> Bool -> Text -> RelayURI -> AppState -> AppState
handleConfirmation eventId' accepted' msg relayURI' st =
  let updateConfirmation = EventConfirmation
        { relay = relayURI'
        , waitingForConfirmation = False
        , accepted = accepted'
        , message = msg
        }

      updateConfirmations :: [EventConfirmation] -> [EventConfirmation]
      updateConfirmations [] = [updateConfirmation]
      updateConfirmations (conf:confs)
        | relay conf == relayURI' && waitingForConfirmation conf =
            updateConfirmation : confs
        | otherwise = conf : updateConfirmations confs
  in st  { confirmations = Map.alter
         (\case
           Nothing -> Just [updateConfirmation]
           Just confs -> Just $ updateConfirmations confs)
         eventId'
         (confirmations st)
      }
