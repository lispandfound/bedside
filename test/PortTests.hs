{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the modules ported from the Python widget sources:
-- Mewo's state machine, seasons, sunrise/sunset math, the WMO code
-- table, and the embedded assets.
module PortTests (tests) where

import Bedside.Assets qualified as Assets
import Bedside.Mewo qualified as Mewo
import Bedside.Season
import Bedside.Sun
import Bedside.Weather
import Codec.Picture (Image (..), PixelRGBA8 (..))
import Codec.Picture.Types (pixelFold)
import Data.Aeson (eitherDecode)
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "ported modules"
    [ mewoTests,
      seasonTests,
      sunTests,
      weatherTests,
      assetTests
    ]

mewoTests :: TestTree
mewoTests =
  testGroup
    "Mewo"
    [ testCase "wandering while awake redraws" $
        Mewo.step (Mewo.Wander Mewo.Desk) Mewo.initial
          @?= (Mewo.Mewo (Just Mewo.Desk) False, Just Mewo.Desk),
      testCase "wandering while asleep does nothing" $ do
        let asleep = Mewo.Mewo (Just Mewo.Sleeping) True
        Mewo.step (Mewo.Wander Mewo.Floor) asleep @?= (asleep, Nothing),
      testCase "bedtime draws the sleeping pose once" $ do
        let (cat, draw) = Mewo.step Mewo.Bedtime (Mewo.Mewo (Just Mewo.Desk) False)
        draw @?= Just Mewo.Sleeping
        Mewo.asleep cat @?= True,
      testCase "bedtime is idempotent" $ do
        let (cat, _) = Mewo.step Mewo.Bedtime Mewo.initial
        Mewo.step Mewo.Bedtime cat @?= (cat, Nothing),
      testCase "bedtime skips redraw if already posed sleeping" $
        snd (Mewo.step Mewo.Bedtime (Mewo.Mewo (Just Mewo.Sleeping) False)) @?= Nothing,
      testCase "waking re-enables wandering without a redraw" $ do
        let (cat, _) = Mewo.step Mewo.Bedtime Mewo.initial
            (awake, draw) = Mewo.step Mewo.WakeUp cat
        draw @?= Nothing
        snd (Mewo.step (Mewo.Wander Mewo.Floor) awake) @?= Just Mewo.Floor
    ]

seasonTests :: TestTree
seasonTests =
  testGroup
    "Season"
    [ testCase "southern-hemisphere season table" $
        map seasonOfMonth [1 .. 12]
          @?= [ Summer, Summer, Autumn, Autumn, Autumn, Winter,
                Winter, Winter, Spring, Spring, Spring, Summer
              ],
      testCase "bert blooms in the warm half" $ do
        bertVariant Summer @?= Bloom
        bertVariant Spring @?= Bloom
        bertVariant Autumn @?= Leafless
        bertVariant Winter @?= Leafless
    ]

-- Reference values generated with the Python suntime package (the
-- library being ported), Christchurch NZ and London:
--   2026-01-01 chch: rise 16:52:12Z set 08:13:12Z
--   2026-07-05 chch: rise 20:03:00Z set 05:04:48Z
--   2026-06-21 london: rise 03:43:12Z set 20:21:00Z
sunTests :: TestTree
sunTests =
  testGroup
    "Sun"
    [ timeOfDayCase "chch new year sunrise" (sunriseUtc (day 2026 1 1) chchLat chchLon) (16 * 3600 + 52 * 60 + 12),
      timeOfDayCase "chch new year sunset" (sunsetUtc (day 2026 1 1) chchLat chchLon) (8 * 3600 + 13 * 60 + 12),
      timeOfDayCase "chch midwinter sunrise" (sunriseUtc (day 2026 7 5) chchLat chchLon) (20 * 3600 + 3 * 60),
      timeOfDayCase "chch midwinter sunset" (sunsetUtc (day 2026 7 5) chchLat chchLon) (5 * 3600 + 4 * 60 + 48),
      timeOfDayCase "london solstice sunrise" (sunriseUtc (day 2026 6 21) 51.5074 (-0.1278)) (3 * 3600 + 43 * 60 + 12),
      timeOfDayCase "london solstice sunset" (sunsetUtc (day 2026 6 21) 51.5074 (-0.1278)) (20 * 3600 + 21 * 60),
      testCase "next events are strictly in the future" $ do
        let now = UTCTime (day 2026 7 5) (12 * 3600)
            Just rise = nextSunrise now chchLat chchLon
            Just set = nextSunset now chchLat chchLon
        assertBool "sunrise after now" (rise > now)
        assertBool "sunset after now" (set > now)
        assertBool "sunrise within a day" (rise < addUTCTime 86400 now)
        assertBool "sunset within a day" (set < addUTCTime 86400 now)
    ]
  where
    chchLat = -43.5321
    chchLon = 172.6362
    day y m d = fromGregorian y m d
    -- Compare wall-clock UTC seconds, ignoring which day the algorithm
    -- anchored the event to; allow two minutes of drift.
    timeOfDayCase name result expected = testCase name $ case result of
      Nothing -> assertFailure "no event computed"
      Just t -> do
        let seconds = round (utctDayTime t) `mod` 86400 :: Integer
            diff = abs (seconds - expected)
            wrapped = min diff (86400 - diff)
        assertBool ("off by " <> show wrapped <> "s") (wrapped <= 120)

weatherTests :: TestTree
weatherTests =
  testGroup
    "Weather"
    [ testCase "WMO code table" $ do
        fromWmoCode 0 @?= Sunny
        fromWmoCode 1 @?= Cloudy
        fromWmoCode 2 @?= Cloudy
        fromWmoCode 3 @?= Overcast
        mapM_ (\c -> fromWmoCode c @?= Rain) [51, 53, 55, 56, 57, 61, 63, 65, 66, 67]
        fromWmoCode 95 @?= Sunny,
      testCase "decodes a real open-meteo response shape" $ do
        let payload =
              "{\"latitude\":-43.5,\"longitude\":172.625,\"timezone\":\"Pacific/Auckland\",\
              \\"daily_units\":{\"time\":\"iso8601\",\"weather_code\":\"wmo code\"},\
              \\"daily\":{\"time\":[\"2026-07-05\"],\"weather_code\":[61]}}"
        case eitherDecode payload of
          Left err -> assertFailure err
          Right forecast -> fromWmoCode (forecastCode forecast) @?= Rain
    ]

assetTests :: TestTree
assetTests =
  testGroup
    "Assets"
    [ testCase "all assets decode at canvas size" $ do
        let images =
              [Assets.background, Assets.night]
                <> map Assets.mewo [minBound .. maxBound]
                <> map Assets.bert [Bloom, Leafless]
                <> [img | Just img <- map Assets.weather [Cloudy, Overcast, Rain]]
        mapM_ (\img -> (imageWidth img, imageHeight img) @?= (800, 480)) images,
      testCase "sunny weather has no artwork" $
        case Assets.weather Sunny of
          Nothing -> pure ()
          Just _ -> assertFailure "expected Nothing",
      testCase "overlay assets carry real transparency" $ do
        -- If BMP alpha were dropped, overlays would be fully opaque
        -- and blot out the background.
        let counts img = pixelFold (\(t, o) _ _ (PixelRGBA8 _ _ _ a) ->
              if a == 0 then (t + 1 :: Int, o) else (t, o + 1 :: Int)) (0, 0) img
            (transparent, opaque) = counts (Assets.mewo Mewo.Desk)
        assertBool "has transparent pixels" (transparent > 0)
        assertBool "has opaque pixels" (opaque > 0)
    ]
