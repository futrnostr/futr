{-# LANGUAGE LambdaCase #-}

module Nostr.Util where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (formatTime, defaultTimeLocale, getCurrentTimeZone, utcToLocalTime, localDay)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get)
import System.Random.Shuffle (shuffleM)

import Nostr.Keys (KeyPair)
import Types (AppState(..), Language(..))

-- | Effect for generating unique IDs and time utilities.
data Util :: Effect where
  GetCurrentTime :: Util m Int
  GetKeyPair :: Util m KeyPair
  ShuffleList :: [a] -> Util m [a]
  FormatDateTime :: Language -> Int -> Util m Text

type instance DispatchOf Util = Dynamic


-- | Effectful type for Util.
type UtilEff es = (State AppState :> es, IOE :> es)


getCurrentTime :: Util :> es => Eff es Int
getCurrentTime = send GetCurrentTime

getKeyPair :: Util :> es => Eff es KeyPair
getKeyPair = send GetKeyPair

shuffleList :: Util :> es => [a] -> Eff es [a]
shuffleList xs = send $ ShuffleList xs

formatDateTime :: Util :> es => Language -> Int -> Eff es Text
formatDateTime lang timestamp = send $ FormatDateTime lang timestamp


-- | Handler for the Util effect.
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

  FormatDateTime lang messageTimestamp -> liftIO $ do
    let utcTime = posixSecondsToUTCTime $ fromIntegral messageTimestamp
    tz <- getCurrentTimeZone
    now <- getZonedTime
    let currentLocalTime = zonedTimeToLocalTime now
        messageLocalTime = utcToLocalTime tz utcTime
        isToday = localDay currentLocalTime == localDay messageLocalTime
        format = case lang of
            English -> if isToday then "%I:%M %p" else "%b %d, %I:%M %p"
            German -> if isToday then "%H:%M" else "%d.%m., %H:%M"
            Spanish -> if isToday then "%H:%M" else "%d/%m, %H:%M"
    return $ T.pack $ formatTime defaultTimeLocale format messageLocalTime
