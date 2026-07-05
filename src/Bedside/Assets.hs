{-# LANGUAGE TemplateHaskell #-}

-- | The BMP artwork, embedded in the binary so deployment is a single
-- file. All assets are full 800x480 canvases with alpha.
module Bedside.Assets
  ( background,
    mewo,
    weather,
    night,
    bert,
  )
where

import Bedside.Mewo qualified as Mewo
import Bedside.Season (BertVariant (..))
import Bedside.Weather (Weather (..))
import Codec.Picture (Image, PixelRGBA8, convertRGBA8, decodeBitmap)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

background :: Image PixelRGBA8
background = decodeAsset "background" $(embedFile "bedside/assets/background.bmp")

mewo :: Mewo.Pose -> Image PixelRGBA8
mewo Mewo.Sleeping = decodeAsset "mewo/sleep" $(embedFile "bedside/assets/mewo/sleep.bmp")
mewo Mewo.Desk = decodeAsset "mewo/desk" $(embedFile "bedside/assets/mewo/desk.bmp")
mewo Mewo.Floor = decodeAsset "mewo/floor" $(embedFile "bedside/assets/mewo/floor.bmp")

-- | Nothing for sunny weather: nothing is drawn, matching Python's
-- blank widget.
weather :: Weather -> Maybe (Image PixelRGBA8)
weather Sunny = Nothing
weather Cloudy = Just (decodeAsset "weather/cloudy" $(embedFile "bedside/assets/weather/cloudy.bmp"))
weather Overcast = Just (decodeAsset "weather/overcast" $(embedFile "bedside/assets/weather/overcast.bmp"))
weather Rain = Just (decodeAsset "weather/rain" $(embedFile "bedside/assets/weather/rain.bmp"))

night :: Image PixelRGBA8
night = decodeAsset "weather/night" $(embedFile "bedside/assets/weather/night.bmp")

bert :: BertVariant -> Image PixelRGBA8
bert Bloom = decodeAsset "bert/bloom" $(embedFile "bedside/assets/bert/bloom.bmp")
bert Leafless = decodeAsset "bert/leafless" $(embedFile "bedside/assets/bert/leafless.bmp")

-- Assets are embedded and covered by tests, so a decode failure is a
-- build defect, not a runtime condition worth threading Either through
-- the app for.
decodeAsset :: String -> ByteString -> Image PixelRGBA8
decodeAsset name bytes = case decodeBitmap bytes of
  Left err -> error ("asset " <> name <> ": " <> err)
  Right dynamic -> convertRGBA8 dynamic
