{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Futr where

import Control.Monad (forM_,void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TQueue, atomically, readTQueue, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import EffectfulQML
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Text.Read (readMaybe)

import Nostr.Effects.Logging
import Nostr.Effects.RelayPool
import Nostr.Keys (KeyPair, PubKeyXO, secKeyToKeyPair)
import Nostr.Types (Event(..), EventId, RelayURI, Response(..))
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

data FutrState = FutrState
  { keyPair :: Maybe KeyPair
  , objRef :: Maybe (ObjRef ())
  , currentScreen :: AppScreen
  , events :: Map EventId (Event, [RelayURI])
  , chats :: Map PubKeyXO [ChatMessage]
  }

initialState :: FutrState
initialState = FutrState
  { keyPair = Nothing
  , objRef = Nothing
  , currentScreen = KeyMgmt
  , events = Map.empty
  , chats = Map.empty
  }

  -- | Futr Effect for managing the application state.
data Futr :: Effect where
  StoreEvent :: Event -> RelayURI -> Futr m ()
  Login :: ObjRef () -> Text -> Futr m ()

type instance DispatchOf Futr = Dynamic

makeEffect ''Futr

-- | Key Management Effect for creating QML UI.
data FutrUI :: Effect where
  CreateUI :: SignalKey (IO ()) -> FutrUI m (ObjRef ())

type instance DispatchOf FutrUI = Dynamic

makeEffect ''FutrUI

type FutrEff es = (State FutrState :> es
                  , PKeyMgmt.KeyMgmt :> es
                  , PKeyMgmt.KeyMgmtUI :> es
                  , RelayPool :> es
                  , State PKeyMgmt.KeyMgmtState :> es
                  , State RelayPoolState :> es
                  , EffectfulQML :> es
                  , Logging :> es
                  , IOE :> es)

runFutr :: FutrEff es => Eff (Futr : es) a -> Eff es a
runFutr = interpret $ \_ -> \case
  StoreEvent event relay -> do
    modify $ \st -> st { events = Map.insert (eventId event) (event, [relay]) (events st) }

  Login obj input -> do
      kst <- get @PKeyMgmt.KeyMgmtState
      case Map.lookup (PKeyMgmt.AccountId input) (PKeyMgmt.accountMap kst) of
        Just a -> do
          modify $ \st' -> st' { keyPair = Just $ secKeyToKeyPair $ PKeyMgmt.nsec a, currentScreen = Home }
          fireSignal obj
          st <- get @RelayPoolState
          let rs = relays st
          forM_ rs $ \(r, _) -> do
            addRelay r
            connect r
          subId' <- subscribe [] $ map fst rs
          --void $ forkIO $ processSubscriptionQueue
          logDebug "Login completed"
        Nothing ->
            return ()

runFutrUI :: FutrEff es => Eff (FutrUI : Futr : es) a -> Eff (Futr : es) a
runFutrUI = interpret $ \_ -> \case
  CreateUI changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ PKeyMgmt.createUI changeKey'

    rootClass <- newClass [
        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertySigRW' "currentScreen" changeKey'
            (\_ -> do
              st <- runE $ get :: IO FutrState
              return $ pack $ show $ currentScreen st)
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        runE $ do
                          modify $ \st -> st { currentScreen = s }
                          fireSignal obj
                    Nothing -> return ()),

        defMethod' "login" $ \obj input -> runE $ login obj input
        ]

    rootObj <- newObject rootClass ()

    runE $ modify $ \st' -> st' { objRef = Just rootObj }

    return rootObj

processSubscriptionQueue :: (Concurrent :> es, Logging :> es, State RelayPoolState :> es) => TQueue Response -> Eff es ()
processSubscriptionQueue queue = do
  let loop = do
        response <- atomically $ readTQueue queue
        case response of
          Closed _ _ -> logDebug "Subscription queue received Closed response, stopping processing"
          _ -> do
            logDebug $ "Received: " <> pack (show response)
            loop
  loop
