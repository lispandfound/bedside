-- | Wiring: the widget map, the update queue, and the background jobs
-- that push new widgets onto it.
module Bedside.App
  ( run,
  )
where

import Bedside.Config
  ( Config (..),
    Location (..),
    ScheduleConfig (..),
    WidgetToggles (..),
  )
import Bedside.Display (Display (..))
import Bedside.Frame (toFrame)
import Bedside.Log (logError, logInfo)
import Bedside.Mewo qualified as Mewo
import Bedside.Schedule (daily, hourlyAt, sleepUntil)
import Bedside.Season (Season, seasonOfMonth)
import Bedside.Sun (nextSunrise, nextSunset)
import Bedside.Weather (fetchWeather)
import Bedside.Widget (Widget (..), composite)
import Bedside.Widgets
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, forM_, void)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Random (randomRIO)

run :: Display -> Config -> IO ()
run display config = do
  manager <- newTlsManager
  queue <- newTBQueueIO 10
  mewoVar <- newTVarIO Mewo.initial

  let Location latitude longitude = configLocation config
      toggles = configWidgets config
      schedule = configSchedule config

  widgets <- initialWidgets manager mewoVar toggles schedule latitude longitude

  mewoJobs <-
    if toggleMewo toggles
      then do
        wanderMinute <- maybe (randomRIO (0, 59)) pure (mewoWanderMinute schedule)
        logInfo ("mewo wanders hourly at minute " <> show wanderMinute)
        pure
          [ hourlyAt wanderMinute (wander queue mewoVar),
            daily (mewoBedtime schedule) (mewoEvent queue mewoVar Mewo.Bedtime),
            daily (mewoWake schedule) (mewoEvent queue mewoVar Mewo.WakeUp)
          ]
      else pure []

  let bertJobs =
        [ daily (bertRefresh schedule) (job "bert" (pushWidget queue . bertWidget =<< currentSeason))
        | toggleBert toggles
        ]
      weatherJobs = [sunLoop manager queue latitude longitude | toggleWeather toggles]

  forM_ (mewoJobs <> bertJobs <> weatherJobs) (void . forkIO)

  eventLoop display queue widgets

-- | Render, then wait for the next widget and go again. Runs on the
-- caller's thread forever.
eventLoop :: Display -> TBQueue Widget -> [Widget] -> IO ()
eventLoop display queue = go . Map.fromList . map (\w -> (widgetName w, w))
  where
    go widgets = do
      logInfo ("refreshing display with " <> show (Map.size widgets) <> " widgets")
      job "refresh" (refresh display (toFrame (composite (Map.elems widgets))))
      next <- atomically (readTBQueue queue)
      logInfo ("widget update: " <> T.unpack (widgetName next))
      go (Map.insert (widgetName next) next widgets)

-- | Mirror of Python @initialise@: background, mewo (awake or asleep
-- by wall clock), today's weather if it can be fetched, and bert —
-- each included only if its toggle is on.
initialWidgets :: Manager -> TVar Mewo.Mewo -> WidgetToggles -> ScheduleConfig -> Double -> Double -> IO [Widget]
initialWidgets manager mewoVar toggles schedule latitude longitude = do
  mewo <-
    if not (toggleMewo toggles)
      then pure Nothing
      else do
        now <- localTimeOfDay . zonedTimeToLocalTime <$> getZonedTime
        if isAwake schedule now
          then do
            pose <- randomPose
            (_, drawn) <- stepMewo mewoVar (Mewo.Wander pose)
            pure (mewoWidget <$> drawn)
          else do
            (_, drawn) <- stepMewo mewoVar Mewo.Bedtime
            pure (mewoWidget <$> drawn)

  weatherW <-
    if not (toggleWeather toggles)
      then pure Nothing
      else do
        weather <- try (fetchWeather manager latitude longitude)
        case weather of
          Right w -> pure (Just (weatherWidget w))
          Left e -> do
            logError ("initial weather fetch failed: " <> show (e :: SomeException))
            pure Nothing

  bert <-
    if toggleBert toggles
      then Just . bertWidget <$> currentSeason
      else pure Nothing

  pure
    ( [backgroundWidget | toggleBackground toggles]
        <> maybe [] pure mewo
        <> maybe [] pure weatherW
        <> maybe [] pure bert
    )

-- | Between the configured wake and bedtime times, i.e. the window
-- mewo wanders in rather than sleeps through. Assumes wake precedes
-- bedtime within the day, which holds for the (and any sane) default.
isAwake :: ScheduleConfig -> TimeOfDay -> Bool
isAwake schedule now = mewoWake schedule <= now && now < mewoBedtime schedule

-- Jobs -----------------------------------------------------------------

wander :: TBQueue Widget -> TVar Mewo.Mewo -> IO ()
wander queue mewoVar = do
  pose <- randomPose
  mewoEvent queue mewoVar (Mewo.Wander pose)

mewoEvent :: TBQueue Widget -> TVar Mewo.Mewo -> Mewo.Event -> IO ()
mewoEvent queue mewoVar event = job ("mewo " <> show event) $ do
  (_, drawn) <- stepMewo mewoVar event
  forM_ drawn (pushWidget queue . mewoWidget)

stepMewo :: TVar Mewo.Mewo -> Mewo.Event -> IO (Mewo.Mewo, Maybe Mewo.Pose)
stepMewo mewoVar event = atomically $ do
  cat <- readTVar mewoVar
  let (cat', drawn) = Mewo.step event cat
  writeTVar mewoVar cat'
  pure (cat', drawn)

randomPose :: IO Mewo.Pose
randomPose = toEnum <$> randomRIO (fromEnum (minBound :: Mewo.Pose), fromEnum (maxBound :: Mewo.Pose))

-- | At sunrise show the day's weather, at sunset the night sky;
-- recompute both a little after the later of the two, forever. This is
-- the legible version of Python's self-rescheduling @scheduler.once@.
sunLoop :: Manager -> TBQueue Widget -> Double -> Double -> IO ()
sunLoop manager queue latitude longitude = forever $ do
  now <- getCurrentTime
  case (,) <$> nextSunrise now latitude longitude <*> nextSunset now latitude longitude of
    Nothing -> do
      logError "no sunrise/sunset here today (polar?); retrying in 6h"
      threadDelay (6 * 3600 * 1_000_000)
    Just (sunrise, sunset) -> do
      logInfo ("next sunrise " <> show sunrise <> ", next sunset " <> show sunset)
      let events =
            [ (sunrise, job "sunrise weather" (pushWidget queue . weatherWidget =<< fetchWeather manager latitude longitude)),
              (sunset, job "sunset night" (pushWidget queue nightWidget))
            ]
      forM_ (sortOnTime events) $ \(at, action) -> sleepUntil at >> action
      sleepUntil (addUTCTime (5 * 60) (max sunrise sunset))
  where
    sortOnTime [a@(ta, _), b@(tb, _)] = if ta <= tb then [a, b] else [b, a]
    sortOnTime es = es

currentSeason :: IO Season
currentSeason = do
  (_, month, _) <- toGregorian . localDay . zonedTimeToLocalTime <$> getZonedTime
  pure (seasonOfMonth month)

pushWidget :: TBQueue Widget -> Widget -> IO ()
pushWidget queue = atomically . writeTBQueue queue

-- | Run an action, logging instead of propagating failure: one bad
-- refresh or fetch must not kill the loops.
job :: String -> IO () -> IO ()
job name action = do
  result <- try action
  case result of
    Right () -> pure ()
    Left e -> logError (name <> ": " <> show (e :: SomeException))
