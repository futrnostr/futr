{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Effects.CurrentTime where

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH

-- | Effect for generating unique IDs.
data CurrentTime :: Effect where
  Now :: CurrentTime m Int

type instance DispatchOf CurrentTime = Dynamic

makeEffect ''CurrentTime

-- | Handler for the IDGen effect.
runCurrentTime
  :: IOE :> es
  => Eff (CurrentTime : es) a
  -> Eff es a
runCurrentTime = interpret $ \_ -> \case
  Now -> do
    n <- liftIO $ fmap (floor . (realToFrac :: POSIXTime -> Double)) getPOSIXTime
    return n
