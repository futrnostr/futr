module TimeFormatter (formatDateTime) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data Language = English | Spanish | German

-- Convert Int (Unix timestamp) to UTCTime
intToUTCTime :: Int-> UTCTime
intToUTCTime = posixSecondsToUTCTime . fromIntegral

-- Format UTCTime to Text based on the provided format
formatUTCTime :: String -> UTCTime -> Text
formatUTCTime format utcTime = T.pack $ formatTime defaultTimeLocale format utcTime

-- Helper function to format time
formatDateTime :: Language -> Int -> Text
formatDateTime English = formatUTCTime "%H:%M:%S %Y-%m-%d" . intToUTCTime
formatDateTime German  = formatUTCTime "%H:%M:%S %d.%m.%Y" . intToUTCTime
formatDateTime Spanish = formatUTCTime "%H:%M:%S %d/%m/%Y" . intToUTCTime
