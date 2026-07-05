-- | Sunrise\/sunset from the 1990 Almanac for Computers algorithm, a
-- port of the @suntime@ package the Python version used. Approximate
-- (within a couple of minutes), which is plenty for scheduling a
-- weather refresh.
module Bedside.Sun
  ( sunriseUtc,
    sunsetUtc,
    nextSunrise,
    nextSunset,
  )
where

import Data.List (find)
import Data.Time
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)

-- | Sunrise on the given UTC day; Nothing if the sun never rises
-- there that day.
sunriseUtc :: Day -> Double -> Double -> Maybe UTCTime
sunriseUtc = sunEvent True

-- | Sunset on the given UTC day; Nothing if the sun never sets.
sunsetUtc :: Day -> Double -> Double -> Maybe UTCTime
sunsetUtc = sunEvent False

-- | First sunrise strictly after the given instant. Nothing only in
-- polar conditions.
nextSunrise :: UTCTime -> Double -> Double -> Maybe UTCTime
nextSunrise = nextEvent sunriseUtc

nextSunset :: UTCTime -> Double -> Double -> Maybe UTCTime
nextSunset = nextEvent sunsetUtc

nextEvent :: (Day -> Double -> Double -> Maybe UTCTime) -> UTCTime -> Double -> Double -> Maybe UTCTime
nextEvent event now latitude longitude = find (> now) candidates
  where
    today = utctDay now
    -- The algorithm anchors events to the reference day loosely enough
    -- that (especially near the date line) the event for day N can land
    -- on day N-1 or N+1; scanning a window and taking the first future
    -- instant sidesteps the anchoring question entirely.
    candidates =
      [ t
      | offset <- [-1 .. 2],
        Just t <- [event (addDays offset today) latitude longitude]
      ]

sunEvent :: Bool -> Day -> Double -> Double -> Maybe UTCTime
sunEvent isRise day latitude longitude =
  attach <$> utHours
  where
    attach h = addUTCTime (realToFrac (h * 3600)) (UTCTime day 0)

    toRad d = d * pi / 180
    yday = fromIntegral (snd (toOrdinalDate day))

    lngHour = longitude / 15
    t
      | isRise = yday + ((6 - lngHour) / 24)
      | otherwise = yday + ((18 - lngHour) / 24)

    -- Sun's mean anomaly and true longitude
    m = (0.9856 * t) - 3.289
    l = forceRange 360 (m + (1.916 * sin (toRad m)) + (0.020 * sin (toRad (2 * m))) + 282.634)

    -- declination
    sinDec = 0.39782 * sin (toRad l)
    cosDec = cos (asin sinDec)

    -- local hour angle at the given zenith
    zenith = 90.8
    cosH = (cos (toRad zenith) - sinDec * sin (toRad latitude)) / (cosDec * cos (toRad latitude))

    -- right ascension, shifted into the same quadrant as L, in hours
    ra0 = forceRange 360 ((180 / pi) * atan (0.91764 * tan (toRad l)))
    lQuadrant = fromIntegral (floor (l / 90) :: Int) * 90
    raQuadrant = fromIntegral (floor (ra0 / 90) :: Int) * 90
    ra = (ra0 + (lQuadrant - raQuadrant)) / 15

    utHours
      | cosH > 1 || cosH < -1 = Nothing
      | otherwise = Just ut
      where
        h
          | isRise = (360 - (180 / pi) * acos cosH) / 15
          | otherwise = ((180 / pi) * acos cosH) / 15
        localMean = h + ra - (0.06571 * t) - 6.622
        utRaw = localMean - lngHour
        ut
          | isRise = forceRange 24 utRaw
          | otherwise = utRaw

forceRange :: Double -> Double -> Double
forceRange bound v
  | v < 0 = v + bound
  | v >= bound = v - bound
  | otherwise = v
