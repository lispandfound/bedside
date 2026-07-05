{-# LANGUAGE OverloadedStrings #-}

module ConfigTests (tests) where

import Bedside.Config
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Time (TimeOfDay (..))
import Test.Tasty
import Test.Tasty.HUnit

decode' :: ByteString -> Either String Config
decode' = eitherDecode

tests :: TestTree
tests =
  testGroup
    "Config"
    [ testCase "location only: everything else defaults" $
        case decode' "{\"location\": {\"latitude\": -43.5, \"longitude\": 172.6}}" of
          Left err -> assertFailure err
          Right config -> do
            configLocation config @?= Location (-43.5) 172.6
            configWidgets config @?= defaultWidgetToggles
            configSchedule config @?= defaultScheduleConfig,
      testCase "full config overrides every field" $
        case decode' fullConfig of
          Left err -> assertFailure err
          Right config -> do
            configLocation config @?= Location 1.0 2.0
            configWidgets config
              @?= WidgetToggles
                { toggleBackground = False,
                  toggleMewo = False,
                  toggleWeather = True,
                  toggleBert = False
                }
            configSchedule config
              @?= ScheduleConfig
                { mewoBedtime = TimeOfDay 22 30 0,
                  mewoWake = TimeOfDay 6 15 0,
                  mewoWanderMinute = Just 42,
                  bertRefresh = TimeOfDay 3 0 0
                },
      testCase "partial widgets object keeps other toggles at default" $
        case decode' "{\"location\": {\"latitude\": 0, \"longitude\": 0}, \"widgets\": {\"mewo\": false}}" of
          Left err -> assertFailure err
          Right config ->
            configWidgets config
              @?= defaultWidgetToggles {toggleMewo = False},
      testCase "partial schedule object keeps other fields at default" $
        case decode' "{\"location\": {\"latitude\": 0, \"longitude\": 0}, \"schedule\": {\"mewoWanderMinute\": 5}}" of
          Left err -> assertFailure err
          Right config ->
            configSchedule config
              @?= defaultScheduleConfig {mewoWanderMinute = Just 5},
      testCase "missing location fails to decode" $
        case decode' "{\"widgets\": {\"mewo\": false}}" of
          Left _ -> pure ()
          Right _ -> assertFailure "expected a decode failure",
      testCase "TimeOfDay strings parse as expected" $
        case decode' "{\"location\": {\"latitude\": 0, \"longitude\": 0}, \"schedule\": {\"mewoBedtime\": \"23:15:00\"}}" of
          Left err -> assertFailure err
          Right config -> mewoBedtime (configSchedule config) @?= TimeOfDay 23 15 0
    ]
  where
    fullConfig =
      "{\
      \  \"location\": {\"latitude\": 1.0, \"longitude\": 2.0},\
      \  \"widgets\": {\"background\": false, \"mewo\": false, \"weather\": true, \"bert\": false},\
      \  \"schedule\": {\
      \    \"mewoBedtime\": \"22:30:00\",\
      \    \"mewoWake\": \"06:15:00\",\
      \    \"mewoWanderMinute\": 42,\
      \    \"bertRefresh\": \"03:00:00\"\
      \  }\
      \}"
