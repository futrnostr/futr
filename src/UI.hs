{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module UI where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (pack, unpack)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (get, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Text.Read (readMaybe)

import Nostr.Bech32
import Nostr.Event
import Nostr.Effects.CurrentTime
import Nostr.Effects.Logging
import Nostr.Effects.RelayPool
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Types (Profile(..), emptyProfile, relayURIToText)
import Presentation.KeyMgmt qualified as PKeyMgmt
import Futr ( Futr, FutrEff, LoginStatusChanged, login, logout, followProfile,
              search, setCurrentProfile, unfollowProfile )
import Types

-- | Key Management Effect for creating QML UI.
data UI :: Effect where
  CreateUI :: SignalKey (IO ()) -> UI m (ObjRef ())


-- | Dispatch type for UI effect.
type instance DispatchOf UI = Dynamic


makeEffect ''UI


-- | Run the UI effect.
runUI :: (FutrEff es, Futr :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ -> \case
  CreateUI changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ PKeyMgmt.createUI changeKey'

    profileClass <- newClass [
        defPropertySigRO' "name" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          let (profile, _) = Map.findWithDefault (emptyProfile, 0) pk (profiles st)
          return $ name profile,

        defPropertySigRO' "displayName" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          let (profile, _) = Map.findWithDefault (emptyProfile, 0) pk (profiles st)
          return $ displayName profile,

        defPropertySigRO' "about" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          let (profile, _) = Map.findWithDefault (emptyProfile, 0) pk (profiles st)
          return $ about profile,

        defPropertySigRO' "picture" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          let (profile, _) = Map.findWithDefault (emptyProfile, 0) pk (profiles st)
          return $ picture profile,

        defPropertySigRO' "nip05" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          let (profile, _) = Map.findWithDefault (emptyProfile, 0) pk (profiles st)
          return $ nip05 profile,

        defPropertySigRO' "banner" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          let (profile, _) = Map.findWithDefault (emptyProfile, 0) pk (profiles st)
          return $ banner profile,

        defPropertySigRO' "isFollow" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let currentPubKey = keyPairToPubKeyXO <$> keyPair st
          let profilePubKey = currentProfile st
          case (currentPubKey, profilePubKey) of
            (Just userPK, Just profilePK) -> do
              let userFollows = Map.findWithDefault [] userPK (followList $ follows st)
              return $ any (\follow -> pubkey follow == profilePK) userFollows
            _ -> return False
        ]

    let followProp name' accessor = defPropertySigRO' name' changeKey' $ \obj -> do
          let pubKeyXO = fromObjRef obj :: PubKeyXO
          st <- runE $ get @AppState
          let followList' = followList $ follows st
          let userPubKey = keyPairToPubKeyXO <$> keyPair st
          let followData = userPubKey >>= \upk -> Map.lookup upk followList' >>= find (\f -> pubkey f == pubKeyXO)
          return $ accessor st followData

    followClass <- newClass [
        followProp "pubkey" $ \_ followMaybe ->
            maybe "" (pubKeyXOToBech32 . pubkey) followMaybe,
        followProp "relay" $ \_ followMaybe ->
            maybe "" (maybe "" relayURIToText . relayURI) followMaybe,
        followProp "petname" $ \_ followMaybe ->
            maybe "" (fromMaybe "" . petName) followMaybe,
        followProp "displayName" $ \st followMaybe ->
            case followMaybe of
              Just follow ->
                let (profile', _) = Map.findWithDefault (emptyProfile, 0) (pubkey follow) (profiles st)
                in fromMaybe "" (displayName profile')
              Nothing -> "",
        followProp "name" $ \st followMaybe ->
            case followMaybe of
              Just follow ->
                let (profile', _) = Map.findWithDefault (emptyProfile, 0) (pubkey follow) (profiles st)
                in fromMaybe "" (name profile')
              Nothing -> "",
        followProp "picture" $ \st followMaybe ->
            case followMaybe of
              Just follow ->
                let (profile', _) = Map.findWithDefault (emptyProfile, 0) (pubkey follow) (profiles st)
                in fromMaybe "" (picture profile')
              Nothing -> ""
      ]

    followPool <- newFactoryPool (newObject followClass)

    rootClass <- newClass [
        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertyConst' "currentProfile" (\_ -> do
          profileObj <- newObject profileClass ()
          runE $ modify @AppState $ \st -> st { profileObjRef = Just profileObj }
          return profileObj
        ),

        defPropertySigRW' "currentScreen" changeKey'
            (\_ -> do
              st <- runE $ get @AppState
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
              let (profile', _) = Map.findWithDefault (emptyProfile, 0) p (profiles st)
              return $ picture profile'
            Nothing -> return Nothing,

        defSignal "loginStatusChanged" (Proxy :: Proxy LoginStatusChanged),

        defMethod' "login" $ \obj input -> runE $ login obj input,

        defMethod' "logout" $ \obj -> runE $ logout obj,

        defMethod' "search" $ \obj input -> runE $ do
          res <- search obj input
          return $ TE.decodeUtf8 $ BSL.toStrict $ encode res,

        defMethod' "setCurrentProfile" $ \_ npub -> runE $ setCurrentProfile npub,

        defMethod' "saveProfile" $ \_ input -> do
          let profile = maybe (error "Invalid profile JSON") id $ decode (BSL.fromStrict $ TE.encodeUtf8 input) :: Profile
          st <- runE $ get @AppState
          n <- runE now
          let kp = maybe (error "No key pair available") id $ keyPair st
          let unsigned = createMetadata profile (keyPairToPubKeyXO kp) n
          signedMaybe <- signEvent unsigned kp
          case signedMaybe of
            Just signed -> do
              st' <- runE $ get @RelayPoolState
              runE $ sendEvent signed $ Map.keys (relays st')
              runE $ logInfo "Profile successfully saved and sent to relay pool"
            Nothing -> runE $ logWarning "Failed to sign profile update event",

        defPropertySigRO' "follows" changeKey' $ \obj -> do
          st <- runE $ get @AppState
          let maybeUserPubKey = keyPairToPubKeyXO <$> keyPair st
          case maybeUserPubKey of
            Just userPubKey -> do
              let userFollows = Map.findWithDefault [] userPubKey (followList $ follows st)
              objs <- mapM (getPoolObject followPool) (map pubkey userFollows)
              runE $ modify $ \s -> s { follows = (follows s) { objRef = Just obj } }
              return objs
            Nothing -> return [],

        defMethod' "follow" $ \_ npubText -> runE $ followProfile npubText,

        defMethod' "unfollow" $ \_ npubText -> runE $ unfollowProfile npubText,

        defMethod' "openChat" $ \obj npubText -> runE $ do
            let npub = npubText
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub
            modify $ \st -> st { currentChatRecipient = Just pubKeyXO }
            fireSignal obj

      ]

    rootObj <- newObject rootClass ()

    return rootObj
