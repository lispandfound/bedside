{-# LANGUAGE OverloadedStrings #-}

{- | The on-disk config: where the display is, which widgets are
turned on, and when the scheduled ones run. Loaded once at startup
from an explicit @--config@ path; changing it means restarting the
app.

Only @location@ is required. Everything else has a default matching
the app's original hardcoded behaviour, so a config with just a
location is already valid.
-}
module Bedside.Config (
    Location (..),
    WidgetToggles (..),
    ScheduleConfig (..),
    Config (..),
    defaultWidgetToggles,
    defaultScheduleConfig,
    loadConfig,
)
where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:), (.:?))
import Data.ByteString.Lazy qualified as BSL
import Data.Time (TimeOfDay (..))

data Location = Location
    { locationLatitude :: Double
    , locationLongitude :: Double
    }
    deriving (Eq, Show)

instance FromJSON Location where
    parseJSON = withObject "location" $ \o ->
        Location <$> o .: "latitude" <*> o .: "longitude"

data WidgetToggles = WidgetToggles
    { toggleBackground :: Bool
    , toggleMewo :: Bool
    , toggleWeather :: Bool
    {- ^ Also gates the sunset "night" widget: day weather and night
    sky share the same display slot.
    -}
    , toggleBert :: Bool
    , toggleArt :: Bool
    }
    deriving (Eq, Show)

defaultWidgetToggles :: WidgetToggles
defaultWidgetToggles =
    WidgetToggles
        { toggleBackground = True
        , toggleMewo = True
        , toggleWeather = True
        , toggleBert = True
        , toggleArt = True
        }

instance FromJSON WidgetToggles where
    parseJSON = withObject "widgets" $ \o ->
        WidgetToggles
            <$> o .:? "background" .!= toggleBackground defaultWidgetToggles
            <*> o .:? "mewo" .!= toggleMewo defaultWidgetToggles
            <*> o .:? "weather" .!= toggleWeather defaultWidgetToggles
            <*> o .:? "bert" .!= toggleBert defaultWidgetToggles
            <*> o .:? "art" .!= toggleArt defaultWidgetToggles

data ScheduleConfig = ScheduleConfig
    { mewoBedtime :: TimeOfDay
    , mewoWake :: TimeOfDay
    , mewoWanderMinute :: Maybe Int
    {- ^ Nothing means pick a random minute each startup, as the app
    always did before this config existed.
    -}
    , bertRefresh :: TimeOfDay
    , artRefresh :: TimeOfDay
    }
    deriving (Eq, Show)

defaultScheduleConfig :: ScheduleConfig
defaultScheduleConfig =
    ScheduleConfig
        { mewoBedtime = TimeOfDay 21 0 0
        , mewoWake = TimeOfDay 7 0 0
        , mewoWanderMinute = Nothing
        , bertRefresh = TimeOfDay 0 0 0
        , artRefresh = TimeOfDay 1 0 0
        }

instance FromJSON ScheduleConfig where
    parseJSON = withObject "schedule" $ \o ->
        ScheduleConfig
            <$> o .:? "mewoBedtime" .!= mewoBedtime defaultScheduleConfig
            <*> o .:? "mewoWake" .!= mewoWake defaultScheduleConfig
            <*> o .:? "mewoWanderMinute"
            <*> o .:? "bertRefresh" .!= bertRefresh defaultScheduleConfig
            <*> o .:? "artRefresh" .!= artRefresh defaultScheduleConfig

data Config = Config
    { configLocation :: Location
    , configWidgets :: WidgetToggles
    , configSchedule :: ScheduleConfig
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON = withObject "config" $ \o ->
        Config
            <$> o .: "location"
            <*> o .:? "widgets" .!= defaultWidgetToggles
            <*> o .:? "schedule" .!= defaultScheduleConfig

{- | Read and parse a config file, failing with a message naming the
file and the parse error.
-}
loadConfig :: FilePath -> IO Config
loadConfig path = do
    bytes <- BSL.readFile path
    case eitherDecode bytes of
        Left err -> ioError (userError ("config " <> path <> ": " <> err))
        Right config -> pure config
