{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Util where

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get)
import Effectful.TH

import Nostr.Keys (KeyPair)
import Types (AppState(..))

-- | Effect for generating unique IDs.
data Util :: Effect where
  GetCurrentTime :: Util m Int
  GetKeyPair :: Util m KeyPair

type instance DispatchOf Util = Dynamic

makeEffect ''Util

-- | Handler for the IDGen effect.
runUtil
  :: (State AppState :> es, IOE :> es)
  => Eff (Util : es) a
  -> Eff es a
runUtil = interpret $ \_ -> \case
  GetCurrentTime -> do
    n <- liftIO $ fmap (floor . (realToFrac :: POSIXTime -> Double)) getPOSIXTime
    return n

  GetKeyPair -> do
    st <- get @AppState
    return $ maybe (error "No key pair found in app state") id $ keyPair st
