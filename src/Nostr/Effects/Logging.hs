{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Effects.Logging where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH

-- | Effect for logging messages.
data Logging :: Effect where
    LogDebug :: Text -> Logging m ()
    LogInfo :: Text -> Logging m ()
    LogError :: Text -> Logging m ()

type instance DispatchOf Logging = Dynamic

makeEffect ''Logging

-- | Handler for the logging effect to stdout.
runLoggingStdout :: IOE :> es => Eff (Logging : es) a -> Eff es a
runLoggingStdout = interpret $ \_ -> \case
    LogDebug msg -> liftIO $ putStrLn ("[DEBUG] " <> show msg)
    LogInfo msg  -> liftIO $ putStrLn ("[INFO] " <> show msg)
    LogError msg -> liftIO $ putStrLn ("[ERROR] " <> show msg)
