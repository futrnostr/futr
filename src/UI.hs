{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text (pack, unpack)
import Data.Text.Encoding qualified as TE
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
import Nostr.Keys (bech32ToPubKeyXO, keyPairToPubKeyXO, pubKeyXOToBech32)
import Nostr.Types (Profile(..), emptyProfile)
import Presentation.KeyMgmt qualified as PKeyMgmt
import Futr (Futr, FutrEff, login)

-- | Key Management Effect for creating QML UI.
data UI :: Effect where
  CreateUI :: SignalKey (IO ()) -> UI m (ObjRef ())

type instance DispatchOf UI = Dynamic

makeEffect ''UI

runUI :: (FutrEff es, Futr :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ -> \case
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
