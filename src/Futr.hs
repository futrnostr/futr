{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Futr where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Text.Read (readMaybe)

import Nostr.Keys (KeyPair, PubKeyXO, secKeyToKeyPair)
import Nostr.Types (Event, EventId, RelayURI)
import Presentation.KeyMgmt qualified as PKeyMgmt

data AppScreen
    = KeyMgmt
    | Relay
    | Home
    deriving (Eq, Read, Show)

data ChatMessage = ChatMessage
  { chatMessageId :: EventId
  , content :: Text
  , author :: PubKeyXO
  , createdAt :: Text
  , seenOn :: [RelayURI]
  }

data Futr = Futr
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen
  , events :: Map EventId (Event, [RelayURI])
  , chats :: Map PubKeyXO [ChatMessage]
  }

initialState :: Futr
initialState = Futr
  { keyPair = Nothing
  , currentScreen = KeyMgmt
  , events = Map.empty
  , chats = Map.empty
  }

-- | Key Management Effect for creating QML UI.
data FutrUI :: Effect where
  CreateCtx :: SignalKey (IO ()) -> FutrUI m (ObjRef ())

type instance DispatchOf FutrUI = Dynamic

makeEffect ''FutrUI

type FutrEff es = (State Futr :> es
                  , PKeyMgmt.KeyMgmt :> es
                  , PKeyMgmt.KeyMgmtUI :> es
                  , State PKeyMgmt.KeyMgmtState :> es
                  , EffectfulQML :> es
                  , IOE :> es)

runFutrUI :: FutrEff es => Eff (FutrUI : es) a -> Eff es a
runFutrUI = interpret $ \_ -> \case
  CreateCtx changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ PKeyMgmt.createUI changeKey'

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
