{-# LANGUAGE OverloadedStrings #-}

-- | Today's weather from Open-Meteo. The Python version computed the
-- timezone from coordinates with tzfpy; Open-Meteo does that itself
-- given @timezone=auto@, so that dependency is gone.
module Bedside.Weather
  ( Weather (..),
    Forecast (..),
    fromWmoCode,
    fetchWeather,
  )
where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.ByteString.Char8 qualified as BS8
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody, setQueryString)

data Weather = Sunny | Cloudy | Overcast | Rain
  deriving (Eq, Show)

-- | WMO weather interpretation codes, same table as the Python port.
-- Unknown codes read as sunny (i.e. nothing drawn).
fromWmoCode :: Int -> Weather
fromWmoCode code = case code of
  0 -> Sunny
  1 -> Cloudy
  2 -> Cloudy
  3 -> Overcast
  c | c `elem` rainCodes -> Rain
  _ -> Sunny
  where
    rainCodes = [51, 53, 55, 56, 57, 61, 63, 65, 66, 67] :: [Int]

-- | Today's WMO weather code, as decoded from the Open-Meteo response.
newtype Forecast = Forecast {forecastCode :: Int}

instance FromJSON Forecast where
  parseJSON = withObject "forecast" $ \o -> do
    daily <- o .: "daily"
    codes <- daily .: "weather_code"
    case codes :: [Int] of
      (code : _) -> pure (Forecast code)
      [] -> fail "daily.weather_code is empty"

-- | Today's forecast for the given coordinates.
fetchWeather :: Manager -> Double -> Double -> IO Weather
fetchWeather manager latitude longitude = do
  request <-
    setQueryString
      [ ("latitude", Just (BS8.pack (show latitude))),
        ("longitude", Just (BS8.pack (show longitude))),
        ("timezone", Just "auto"),
        ("daily", Just "weather_code"),
        ("forecast_days", Just "1")
      ]
      <$> parseRequest "https://api.open-meteo.com/v1/forecast"
  response <- httpLbs request manager
  case eitherDecode (responseBody response) of
    Left err -> ioError (userError ("open-meteo response: " <> err))
    Right (Forecast code) -> pure (fromWmoCode code)
