{-# LANGUAGE BlockArguments #-}

module Futr where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Control.Monad (forM_, void, unless, when)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, cancel, waitAnyCancel, withAsync)
import Effectful.Concurrent.STM (TQueue, atomically, readTQueue, flushTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)

import AppState
import Nostr.Effects.CurrentTime
import Nostr.Effects.Logging
import Nostr.Effects.RelayPool
import Nostr.Keys (keyPairToPubKeyXO, secKeyToKeyPair)
import Nostr.Types (Event(..), EventId(..), Filter(..), Kind(..), Tag(..), RelayURI, Response(..), Relay(..))
import Presentation.KeyMgmt qualified as PKeyMgmt



  -- | Futr Effect for managing the application state.
data Futr :: Effect where
  Login :: ObjRef () -> Text -> Futr m ()

type instance DispatchOf Futr = Dynamic

makeEffect ''Futr

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

runFutr :: FutrEff es => Eff (Futr : es) a -> Eff es a
runFutr = interpret $ \_ -> \case
  Login obj input -> do
      kst <- get @PKeyMgmt.KeyMgmtState
      case Map.lookup (PKeyMgmt.AccountId input) (PKeyMgmt.accountMap kst) of
        Just a -> loginWithAccount obj a
        Nothing -> return ()

loginWithAccount :: FutrEff es => ObjRef () -> PKeyMgmt.Account -> Eff es ()
loginWithAccount obj a = do
  let kp = secKeyToKeyPair $ PKeyMgmt.nsec a
  let xo = keyPairToPubKeyXO kp

  oldState <- gets @AppState id
  let newState = oldState { keyPair = Just kp }
  modify @AppState (const newState)

  let rs = PKeyMgmt.relays a
  mapM_ addRelay rs
  void . async $ mapM_ connect rs
  n <- now
  let initialFilters =
        [ FollowListFilter [ xo ] n
        , MetadataFilter [ xo ] n
        ]

  let runSubscription :: FutrEff es => Relay -> Eff es ()
      runSubscription relay' = do
        maybeSubInfo <- startSubscription (uri relay') initialFilters
        case maybeSubInfo of
          Nothing -> logWarning $ "Failed to start subscription for relay: " <> pack (show (uri relay'))
          Just (subId', queue) -> do
            let loop = do
                  msg <- atomically $ readTQueue queue
                  msgs <- atomically $ flushTQueue queue
                  stopped <- handleResponsesUntilEOSE (uri relay') (msg : msgs)
                  when stopped $ do
                    stopSubscription subId'
                    return ()
                  threadDelay $ 100 * 1000
                  unless stopped loop
            loop

  -- Run all subscriptions concurrently with a 10-second timeout
  withAsync (threadDelay $ 10 * 1000 * 1000) $ \timeout -> do
    subscriptionAsyncs <- mapM (async . runSubscription) rs
    void $ waitAnyCancel (timeout : subscriptionAsyncs)
    mapM_ cancel subscriptionAsyncs

  -- Start the main subscription in the background
  void . async $ forM_ rs \relay' -> do
    st <- get @AppState
    let followedPubKeys = concatMap (\(_, follows') -> map (\(pubKey, _, _) -> pubKey) follows') $ Map.toList (follows st)
    n' <- now
    let filters =
          [ FollowListFilter (xo : followedPubKeys) n'
          , MetadataFilter (xo : followedPubKeys) n'
          ]
    maybeSubInfo <- startSubscription (uri relay') filters
    case maybeSubInfo of
      Nothing -> logWarning $ "Failed to start second subscription for relay: " <> pack (show (uri relay'))
      Just (_, queue) -> do
        void . async $ handleResponsesUntilClosed obj (uri relay') queue

  modify $ \st -> st { currentScreen = Home }
  fireSignal $ Just obj
  return ()

handleResponsesUntilEOSE :: FutrEff es => RelayURI -> [Response] -> Eff es Bool
handleResponsesUntilEOSE _ [] = return False
handleResponsesUntilEOSE relayURI (r:rs) = case r of
  EventReceived _ event' -> do
    handleEvent event'
    handleResponsesUntilEOSE relayURI rs
  Eose _ -> return True
  Closed _ _ -> return True
  Ok eventId' accepted' msg -> do
    modify $ handleConfirmation eventId' accepted' msg relayURI
    handleResponsesUntilEOSE relayURI rs
  Notice msg -> do
    modify $ handleNotice relayURI msg
    handleResponsesUntilEOSE relayURI rs

handleResponsesUntilClosed :: FutrEff es => ObjRef () -> RelayURI -> TQueue Response -> Eff es ()
handleResponsesUntilClosed obj relayURI queue = do
  let loop = do
        msg <- atomically $ readTQueue queue
        msgs <- atomically $ flushTQueue queue
        stopped <- processResponses relayURI (msg : msgs)
        fireSignal Nothing
        threadDelay $ 100 * 1000
        unless stopped loop
  loop

processResponses :: FutrEff es => RelayURI -> [Response] -> Eff es Bool
processResponses relayURI = go
  where
    go [] = return False
    go (r:rs) = case r of
      EventReceived _ event' -> do
        handleEvent event'
        go rs
      Eose subId' -> do
        logDebug $ "EOSE on subscription " <> subId'
        go rs
      Closed subId msg -> do
        logDebug $ "Closed subscription " <> subId <> " with message " <> msg
        return True
      Ok eventId' accepted' msg -> do
        logDebug $ "OK on subscription " <> pack ( show eventId' ) <> " with message " <> msg
        modify $ handleConfirmation eventId' accepted' msg relayURI
        go rs
      Notice msg -> do
        modify $ handleNotice relayURI msg
        go rs

handleEvent :: FutrEff es => Event -> Eff es ()
handleEvent event' = case kind event' of
  Metadata -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 $ content event') of
    Right profile -> modify $ \st ->
      let updateProfile = case Map.lookup (pubKey event') (profiles st) of
            Just (_, oldTime) | (createdAt event') > oldTime -> True
            Nothing -> True
            _ -> False
      in if updateProfile
          then st { profiles = Map.insert (pubKey event') (profile, (createdAt event')) (profiles st) }
          else st
    Left err -> logWarning $ "Failed to decode metadata: " <> pack err

  FollowList -> do
    let followList = [(pubKey', relayUri', displayName') | PTag pubKey' relayUri' displayName' <- tags event']
    modify $ \st -> st { follows = Map.insert (pubKey event') followList (follows st) }

  _ -> return ()

handleNotice :: RelayURI -> Text -> RelayPoolState -> RelayPoolState
handleNotice relayURI msg st =
  st { relays = Map.adjust (\rd -> rd { notices = msg : notices rd }) relayURI (relays st) }


handleConfirmation :: EventId -> Bool -> Text -> RelayURI -> AppState -> AppState
handleConfirmation eventId' accepted' msg relayURI st =
  let updateConfirmation = EventConfirmation
        { relay = relayURI
        , waitingForConfirmation = False
        , accepted = accepted'
        , message = msg
        }

      updateConfirmations :: [EventConfirmation] -> [EventConfirmation]
      updateConfirmations [] = [updateConfirmation]
      updateConfirmations (conf:confs)
        | relay conf == relayURI && waitingForConfirmation conf =
            updateConfirmation : confs
        | otherwise = conf : updateConfirmations confs
  in st  { confirmations = Map.alter
         (\case
           Nothing -> Just [updateConfirmation]
           Just confs -> Just $ updateConfirmations confs)
         eventId'
         (confirmations st)
      }
