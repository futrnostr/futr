{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Effects.IDGen where

import Control.Monad (replicateM)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import System.Random (randomIO)

-- | Effect for generating unique IDs.
data IDGen :: Effect where
  GenerateID :: Int -> IDGen m Text

type instance DispatchOf IDGen = Dynamic

makeEffect ''IDGen

-- | Handler for the IDGen effect.
runIDGen
  :: IOE :> es
  => Eff (IDGen : es) a
  -> Eff es a
runIDGen = interpret $ \_ -> \case
  GenerateID n -> do
    bytes <- liftIO $ replicateM n randomIO
    let byteString = BS.pack bytes
    return $ TE.decodeUtf8 $ B16.encode byteString
