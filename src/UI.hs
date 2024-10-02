{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module UI where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding qualified as TE
import Data.Typeable (Typeable)
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
import Nostr.Types (Profile(..), RelayURI, emptyProfile, relayURIToText)
import Presentation.KeyMgmt qualified as PKeyMgmt
import Futr (Futr, FutrEff, login)

-- | Key Management Effect for creating QML UI.
data UI :: Effect where
  CreateUI :: SignalKey (IO ()) -> UI m (ObjRef ())

type instance DispatchOf UI = Dynamic

makeEffect ''UI

data FollowId = FollowId PubKeyXO Int deriving (Eq, Ord, Show, Typeable)

runUI :: (FutrEff es, Futr :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ -> \case
  CreateUI changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ PKeyMgmt.createUI changeKey'

    let lookupFollowData :: AppState -> FollowId -> Maybe (PubKeyXO, Maybe RelayURI, Maybe Text)
        lookupFollowData st (FollowId userPubKey idx) =
          let userFollows = Map.findWithDefault [] userPubKey (follows st)
          in if idx >= 0 && idx < length userFollows
             then Just (userFollows !! idx)
             else Nothing

    let followProp name' accessor = defPropertySigRO' name' changeKey' $ \obj -> do
          let followId = fromObjRef obj :: FollowId
          st <- runE $ get @AppState
          let followData = lookupFollowData st followId
          return $ accessor st followData

    followClass <- newClass [
        followProp "pubkey" $ \st followData -> 
            fmap (pubKeyXOToBech32 . (\(pubkey, _, _) -> pubkey)) followData,
        followProp "relay" $ \st followData -> 
            case followData of
                Just (_, relayURIMaybe, _) -> relayURIToText <$> relayURIMaybe
                Nothing -> Nothing,
        followProp "petname" $ \st followData -> 
            case followData of
                Just (_, _, petnameMaybe) -> petnameMaybe
                Nothing -> Nothing,
        followProp "displayName" $ \st followData -> 
            fmap (\(pubkey, _, _) -> 
                let (profile', _) = Map.findWithDefault (emptyProfile, 0) pubkey (profiles st)
                in fromMaybe "" (displayName profile')) followData,
        followProp "picture" $ \st followData -> 
            fmap (\(pubkey, _, _) -> 
                let (profile', _) = Map.findWithDefault (emptyProfile, 0) pubkey (profiles st)
                in fromMaybe "" (picture profile')) followData
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
                          fireSignal $ Just obj
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
            Nothing -> runE $ logWarning "Failed to sign profile update event",

        defPropertySigRO' "follows" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let maybeUserPubKey = keyPairToPubKeyXO <$> keyPair st
          case maybeUserPubKey of
            Just userPubKey -> do
              let userFollowsList = Map.findWithDefault [] userPubKey (follows st)
              let followIds = [FollowId userPubKey idx | idx <- [0..(length userFollowsList - 1)]]
              mapM (getPoolObject followPool) followIds
            Nothing -> return [],

        defMethod' "follow" $ \obj npubText -> runE $ do
            let npub = npubText
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub
            st <- get @AppState
            let userPubKey = keyPairToPubKeyXO <$> keyPair st
            case userPubKey of
                Just userPK -> do
                    let currentFollows = Map.findWithDefault [] userPK (follows st)
                    -- Avoid duplicates
                    let newFollows = if any (\(pk, _, _) -> pk == pubKeyXO) currentFollows
                                     then currentFollows
                                     else (pubKeyXO, Nothing, Nothing) : currentFollows
                    modify $ \st' -> st' { follows = Map.insert userPK newFollows (follows st') }
                    -- Optionally, fetch the profile of the new follow
                    -- Update the profiles map if needed
                    fireSignal $ Just obj
                Nothing -> return (),

        defMethod' "unfollow" $ \obj npubText -> runE $ do
            let npub = npubText
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub
            st <- get @AppState
            let userPubKey = keyPairToPubKeyXO <$> keyPair st
            case userPubKey of
                Just userPK -> do
                    let currentFollows = Map.findWithDefault [] userPK (follows st)
                    let newFollows = filter (\(pk, _, _) -> pk /= pubKeyXO) currentFollows
                    modify $ \st' -> st' { follows = Map.insert userPK newFollows (follows st') }
                    fireSignal $ Just obj
                Nothing -> return (),

        defMethod' "openChat" $ \obj npubText -> runE $ do
            let npub = npubText
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub
            modify $ \st -> st { currentChatRecipient = Just pubKeyXO }
            fireSignal $ Just obj
        ]

    rootObj <- newObject rootClass ()

    --runE $ modify $ \st' -> st' { objRef = Just rootObj }

    return rootObj
