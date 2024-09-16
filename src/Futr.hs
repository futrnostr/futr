{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Futr where

import Data.Map.Strict qualified as Map
import Data.Text (pack, unpack)
import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Text.Read (readMaybe)

import Nostr.Keys (KeyPair, secKeyToKeyPair)
import Presentation.KeyMgmt qualified as PKeyMgmt

data AppScreen
    = KeyMgmt
    | Relay
    | Home
    deriving (Eq, Read, Show)

data Futr = Futr
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen
  }

initialState :: Futr
initialState = Futr
  { keyPair = Nothing
  , currentScreen = KeyMgmt
  }

-- | Key Management Effect for creating QML context.
data FutrContext :: Effect where
  CreateCtx :: SignalKey (IO ()) -> FutrContext m (ObjRef ())

type instance DispatchOf FutrContext = Dynamic

makeEffect ''FutrContext

type FutrEff es = (PKeyMgmt.KeyMgmtContext :> es, State Futr :> es, State PKeyMgmt.KeyMgmtState :> es, EffectfulQML :> es, IOE :> es)

runFutrContext :: FutrEff es => Eff (FutrContext : es) a -> Eff es a
runFutrContext action = interpret handleFutrContext action
  where
    handleFutrContext :: FutrEff es => EffectHandler FutrContext es
    handleFutrContext _ = \case
      CreateCtx changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        keyMgmtObj <- runE $ PKeyMgmt.createCtx changeKey'

        rootClass <- newClass [
            defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

            defPropertySigRW' "currentScreen" changeKey'
                (\_ -> do
                  st <- runE $ get :: IO Futr
                  return $ pack $ show $ currentScreen st)
                (\obj newScreen -> do
                    case readMaybe (unpack newScreen) :: Maybe AppScreen of
                        Just s -> do
                            runE $ do
                              modify $ \st -> st { currentScreen = s }
                              fireSignal obj
                        Nothing -> return ()),

            defMethod' "login" $ \obj input -> do
                st <- runE get :: IO PKeyMgmt.KeyMgmtState
                case Map.lookup (PKeyMgmt.AccountId input) (PKeyMgmt.accountMap st) of
                    Just a -> do
                        runE $ do
                          modify $ \st' -> st' { keyPair = Just $ secKeyToKeyPair $ PKeyMgmt.nsec a, currentScreen = Home }
                          fireSignal obj
                    Nothing ->
                        return ()
            ]

        rootObj <- newObject rootClass ()
        return rootObj
