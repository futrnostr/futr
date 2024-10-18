{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Util where

import Control.Monad (replicateM)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import System.Random (randomIO)

-- | Effect for generating unique IDs.
data Util :: Effect where
  GenerateID :: Int -> Util m Text
  GetCurrentTime :: Util m Int

type instance DispatchOf Util = Dynamic

makeEffect ''Util

-- | Handler for the IDGen effect.
runUtil
  :: IOE :> es
  => Eff (Util : es) a
  -> Eff es a
runUtil = interpret $ \_ -> \case

  GenerateID n -> do
    bytes <- liftIO $ replicateM n randomIO
    let byteString = BS.pack bytes
    return $ TE.decodeUtf8 $ B16.encode byteString

  GetCurrentTime -> do
    n <- liftIO $ fmap (floor . (realToFrac :: POSIXTime -> Double)) getPOSIXTime
    return n
