-- | Timestamped stderr logging; all this app needs.
module Bedside.Log
  ( logInfo,
    logError,
  )
where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (hPutStrLn, stderr)

logInfo, logError :: String -> IO ()
logInfo = logWith "INFO"
logError = logWith "ERROR"

logWith :: String -> String -> IO ()
logWith level message = do
  now <- getCurrentTime
  hPutStrLn stderr (formatTime defaultTimeLocale "%F %T" now <> " [" <> level <> "] " <> message)
