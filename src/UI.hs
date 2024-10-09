{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module UI where

import Control.Monad (unless)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.Text.Encoding qualified as TE
import Data.Proxy (Proxy(..))
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (get, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Text.Read (readMaybe)


import AppState
import Nostr.Event
import Nostr.Effects.CurrentTime
import Nostr.Effects.Logging
import Nostr.Effects.RelayPool
import Nostr.Keys (PubKeyXO, bech32ToPubKeyXO, keyPairToPubKeyXO, pubKeyXOToBech32)
import Nostr.Types (Profile(..), emptyProfile, relayURIToText)
import Presentation.KeyMgmt qualified as PKeyMgmt
import Futr (Futr, FutrEff, LoginStatusChanged, login, logout)

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
              let (profile', _) = Map.findWithDefault (emptyProfile, 0) p (profiles st)
              return $ picture profile'
            Nothing -> return Nothing,

        defSignal "loginStatusChanged" (Proxy :: Proxy LoginStatusChanged),

        defMethod' "login" $ \obj input -> runE $ login obj input,

        defMethod' "logout" $ \obj -> runE $ logout obj,

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

        defMethod' "follow" $ \obj npubText -> runE $ do
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npubText
            st <- get @AppState
            case keyPairToPubKeyXO <$> keyPair st of
                Just userPK -> do
                    let currentFollows = Map.findWithDefault [] userPK (followList $ follows st)
                    unless (any (\follow -> pubkey follow == pubKeyXO) currentFollows) $ do
                        let newFollow = Follow pubKeyXO Nothing Nothing
                        let newFollows = newFollow : currentFollows
                        modify $ \st' -> st' { follows = FollowModel (Map.insert userPK newFollows (followList $ follows st')) Nothing }
                    fireSignal obj
                Nothing -> return (),

        defMethod' "unfollow" $ \obj npubText -> runE $ do
            let npub = npubText
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub
            st <- get @AppState
            let userPubKey = keyPairToPubKeyXO <$> keyPair st
            case userPubKey of
                Just userPK -> do
                    let currentFollows = Map.findWithDefault [] userPK (followList $ follows st)
                    let newFollows = filter (\follow -> pubkey follow /= pubKeyXO) currentFollows
                    modify $ \st' -> st' { follows = FollowModel (Map.insert userPK newFollows (followList $ follows st')) Nothing }
                    fireSignal obj
                Nothing -> return (),

        defMethod' "openChat" $ \obj npubText -> runE $ do
            let npub = npubText
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub
            modify $ \st -> st { currentChatRecipient = Just pubKeyXO }
            fireSignal obj
        ]

    rootObj <- newObject rootClass ()

    return rootObj
