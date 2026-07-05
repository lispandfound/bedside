-- | Just enough scheduling for this app: sleep to an instant, run
-- something daily at a local wall-clock time, or hourly at a fixed
-- minute. Replaces the Python @scheduler@ dependency.
module Bedside.Schedule
  ( sleepUntil,
    daily,
    hourlyAt,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Time

sleepUntil :: UTCTime -> IO ()
sleepUntil target = loop
  where
    -- Sleep in bounded chunks and re-check the clock, so NTP jumps and
    -- suspend/resume don't leave us oversleeping.
    chunkMicros = 15 * 60 * 1_000_000
    loop = do
      now <- getCurrentTime
      let remainingMicros = ceiling (diffUTCTime target now * 1_000_000) :: Integer
      when (remainingMicros > 0) $ do
        threadDelay (fromIntegral (min chunkMicros remainingMicros))
        loop

-- | Run the action every day at the given local wall-clock time. The
-- timezone is re-read each day, so DST shifts are picked up.
daily :: TimeOfDay -> IO () -> IO ()
daily tod action =
  forever $ do
    next <- nextLocalOccurrence tod
    sleepUntil next
    action

nextLocalOccurrence :: TimeOfDay -> IO UTCTime
nextLocalOccurrence tod = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localNow = utcToLocalTime tz now
      today = LocalTime (localDay localNow) tod
      next
        | today > localNow = today
        | otherwise = LocalTime (addDays 1 (localDay localNow)) tod
  pure (localTimeToUTC tz next)

-- | Run the action once an hour when the clock reads the given minute.
hourlyAt :: Int -> IO () -> IO ()
hourlyAt minuteMark action =
  forever $ do
    now <- getCurrentTime
    sleepUntil (nextMinuteMark minuteMark now)
    action

nextMinuteMark :: Int -> UTCTime -> UTCTime
nextMinuteMark minuteMark now
  | candidate > now = candidate
  | otherwise = addUTCTime 3600 candidate
  where
    hourStart = UTCTime (utctDay now) (fromIntegral (3600 * (floor (utctDayTime now) `div` 3600 :: Integer)))
    candidate = addUTCTime (fromIntegral (60 * minuteMark)) hourStart
