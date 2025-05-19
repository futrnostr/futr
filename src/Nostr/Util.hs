{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Util where

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get)
import System.Random.Shuffle (shuffleM)

import Nostr.Keys (KeyPair)
import Types (AppState(..))

-- | Effect for generating unique IDs.
data Util :: Effect where
  GetCurrentTime :: Util m Int
  GetKeyPair :: Util m KeyPair
  ShuffleList :: [a] -> Util m [a]

type instance DispatchOf Util = Dynamic


-- | Effectful type for Util.
type UtilEff es = (State AppState :> es, IOE :> es)


getCurrentTime :: Util :> es => Eff es Int
getCurrentTime = send GetCurrentTime

getKeyPair :: Util :> es => Eff es KeyPair
getKeyPair = send GetKeyPair

shuffleList :: Util :> es => [a] -> Eff es [a]
shuffleList xs = send $ ShuffleList xs


-- | Handler for the IDGen effect.
runUtil
  :: UtilEff es
  => Eff (Util : es) a
  -> Eff es a
runUtil = interpret $ \_ -> \case
  GetCurrentTime -> do
    n <- liftIO $ fmap (floor . (realToFrac :: POSIXTime -> Double)) getPOSIXTime
    return n

  GetKeyPair -> do
    st <- get @AppState
    return $ maybe (error "No key pair found in app state") id $ keyPair st

  ShuffleList xs -> liftIO $ shuffleM xs
