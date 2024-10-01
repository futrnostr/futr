{-# LANGUAGE BlockArguments #-}

module Futr where

import Data.Aeson (decode, eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Control.Monad (forM_, void, unless, when)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
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
import Text.Read (readMaybe)

import AppState
import Nostr.Event
import Nostr.Effects.CurrentTime
import Nostr.Effects.Logging
import Nostr.Effects.RelayPool
import Nostr.Keys (bech32ToPubKeyXO, keyPairToPubKeyXO, pubKeyXOToBech32, secKeyToKeyPair)
import Nostr.Types (Event(..), EventId(..), Filter(..), Kind(..), Profile(..), Tag(..), RelayURI, Response(..), Relay(..), emptyProfile)
import Presentation.KeyMgmt qualified as PKeyMgmt


  -- | Futr Effect for managing the application state.
data Futr :: Effect where
  Login :: ObjRef () -> Text -> Futr m ()

type instance DispatchOf Futr = Dynamic

makeEffect ''Futr

-- | Key Management Effect for creating QML UI.
data FutrUI :: Effect where
  CreateUI :: SignalKey (IO ()) -> FutrUI m (ObjRef ())

type instance DispatchOf FutrUI = Dynamic

makeEffect ''FutrUI

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
  -- fiatjaf pub key for testing contact loading
  --let xo' = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"

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
    let followedPubKeys = Map.keys (follows st)
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
  fireSignal obj
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
        fireSignal obj
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


runFutrUI :: FutrEff es => Eff (FutrUI : Futr : es) a -> Eff (Futr : es) a
runFutrUI = interpret $ \_ -> \case
  CreateUI changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ PKeyMgmt.createUI changeKey'

    rootClass <- newClass [
        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertySigRW' "currentScreen" changeKey'
            (\_ -> do
              st <- runE $ get :: IO AppState
              return $ pack $ show $ currentScreen st)
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        runE $ do
                          modify $ \st -> st { currentScreen = s }
                          fireSignal obj
                    Nothing -> return ()),

        defPropertySigRO' "mynpub" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          case keyPair st of
            Just kp -> return $ Just $ pubKeyXOToBech32 $ keyPairToPubKeyXO kp
            Nothing -> return Nothing,

        defPropertySigRO' "mypicture" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          case keyPair st of
            Just kp -> do
              let p = keyPairToPubKeyXO kp
              putStrLn $ "p: " ++ (show p)
              let (profile', x) = Map.findWithDefault (emptyProfile, 0) p (profiles st)
              putStrLn $ "profile: " ++ (show $ profiles st)
              putStrLn $ show x
              return $ picture profile'
            Nothing -> return Nothing,

        defMethod' "login" $ \obj input -> runE $ login obj input,

        defMethod' "getProfile" $ \_ npub -> do
          st <- runE $ get @AppState
          let xo = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub
          let (profile', _) = Map.findWithDefault (emptyProfile, 0) xo (profiles st)
          return $ TE.decodeUtf8 $ BSL.toStrict $ encode profile',

        defMethod' "saveProfile" $ \_ input -> do
          let profile = maybe (error "Invalid profile JSON") id $ decode (BSL.fromStrict $ TE.encodeUtf8 input) :: Profile
          st <- runE $ get @AppState
          n <- runE now
          let kp = maybe (error "No key pair available") id $ keyPair st
          let unsigned = setMetadata profile (keyPairToPubKeyXO kp) n
          signedMaybe <- signEvent unsigned kp
          case signedMaybe of
            Just signed -> do
              st' <- runE $ get @RelayPoolState
              runE $ sendEvent signed $ Map.keys (relays st')
              runE $ logInfo "Profile successfully saved and sent to relay pool"
            Nothing -> runE $ logWarning "Failed to sign profile update event"
        ]

    rootObj <- newObject rootClass ()

    runE $ modify $ \st' -> st' { objRef = Just rootObj }

    return rootObj