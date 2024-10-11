module Nostr.Effects.Subscription where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (MVar, putMVar)
import Effectful.Concurrent.STM (TQueue, atomically, writeTQueue)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH

import Nostr.Keys (PubKeyXO)
import Nostr.Effects.Logging
import Nostr.Effects.RelayPool
import Nostr.Types
import Types (AppState(..))

type SubscriptionEff es = (RelayPool :> es, State (RelayPoolState es) :> es, State AppState :> es, Logging :> es, Concurrent :> es)

loadingSubscriptionHandler :: SubscriptionEff es => PubKeyXO -> MVar () -> SubscriptionHandler (Eff es)
loadingSubscriptionHandler xo signal = SubscriptionHandler
  { onEvent = \event' -> case kind event' of
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

      ShortTextNote -> return ()

      FollowList -> do
          let followList = [(pubKey, relayUri, displayName) | PTag pubKey relayUri displayName <- tags event']
          modify $ \st -> st { follows = Map.insert (pubKey event') followList (follows st) }

      _ -> return ()

  , onEOSE = \subId' relayURI -> do
      stopSubscription subId'
      putMVar signal ()
  , onClose = \subId' relayURI msg -> return ()
  }

defaultSubscriptionHandler :: SubscriptionEff es => PubKeyXO -> SubscriptionHandler (Eff es)
defaultSubscriptionHandler xo = SubscriptionHandler
  { onEvent = \event' -> case kind event' of
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

      ShortTextNote -> return ()

      FollowList -> do
          let followList = [(pubKey, relayUri, displayName) | PTag pubKey relayUri displayName <- tags event']
          modify $ \st -> st { follows = Map.insert (pubKey event') followList (follows st) }

      _ -> return ()
  , onEOSE = \subId' relayURI -> return ()
  , onClose = \subId' relayURI msg -> return ()
  }
