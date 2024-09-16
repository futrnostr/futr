module TimeFormatter (formatDateTime) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data Language = English | Spanish | German

-- Convert Int64 (Unix timestamp) to UTCTime
int64ToUTCTime :: Int64 -> UTCTime
int64ToUTCTime = posixSecondsToUTCTime . fromIntegral

-- Format UTCTime to Text based on the provided format
formatUTCTime :: String -> UTCTime -> Text
formatUTCTime format utcTime = T.pack $ formatTime defaultTimeLocale format utcTime

-- Helper function to format time
formatDateTime :: Language -> Int64 -> Text
formatDateTime English = formatUTCTime "%H:%M:%S %Y-%m-%d" . int64ToUTCTime
formatDateTime German  = formatUTCTime "%H:%M:%S %d.%m.%Y" . int64ToUTCTime
formatDateTime Spanish = formatUTCTime "%H:%M:%S %d/%m/%Y" . int64ToUTCTime
