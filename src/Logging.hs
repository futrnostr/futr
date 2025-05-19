{-# LANGUAGE LambdaCase #-}

module Logging where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)

-- | Effect for logging messages.
data Logging :: Effect where
    LogDebug :: Text -> Logging m ()
    LogInfo :: Text -> Logging m ()
    LogWarning :: Text -> Logging m ()
    LogError :: Text -> Logging m ()

type instance DispatchOf Logging = Dynamic


-- | Effectful type for Logging.
type LoggingEff es = (IOE :> es)


logDebug :: Logging :> es => Text -> Eff es ()
logDebug msg = send $ LogDebug msg

logInfo :: Logging :> es => Text -> Eff es ()
logInfo msg = send $ LogInfo msg

logWarning :: Logging :> es => Text -> Eff es ()
logWarning msg = send $ LogWarning msg

logError :: Logging :> es => Text -> Eff es ()
logError msg = send $ LogError msg


-- | Handler for the logging effect to stdout.
runLoggingStdout :: LoggingEff es => Eff (Logging : es) a -> Eff es a
runLoggingStdout = interpret $ \_ -> \case
    LogDebug msg -> liftIO $ putStrLn ("[DEBUG] " <> show msg)
    LogInfo msg  -> liftIO $ putStrLn ("[INFO] " <> show msg)
    LogWarning msg -> liftIO $ putStrLn ("[WARN] " <> show msg)
    LogError msg -> liftIO $ putStrLn ("[ERROR] " <> show msg)
