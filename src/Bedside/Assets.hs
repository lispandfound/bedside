{-# LANGUAGE TemplateHaskell #-}

{- | The BMP artwork, embedded in the binary so deployment is a single
file. All assets are full 800x480 canvases with alpha.
-}
module Bedside.Assets (
    art,
    background,
    mewo,
    weather,
    night,
    bert,
)
where

import Bedside.Mewo qualified as Mewo
import Bedside.Season (BertVariant (..))
import Bedside.Weather (Weather (..))
import Codec.Compression.GZip (decompress)
import Codec.Picture (Image, PixelRGBA8, convertRGBA8, decodeBitmap)
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.FileEmbed (embedFile)

background :: Image PixelRGBA8
background = decodeAsset "background" $(embedFile "assets/background.bmp.gz")

mewo :: Mewo.Pose -> Image PixelRGBA8
mewo Mewo.Sleeping = decodeAsset "mewo/sleep" $(embedFile "assets/mewo/sleep.bmp.gz")
mewo Mewo.Desk = decodeAsset "mewo/desk" $(embedFile "assets/mewo/desk.bmp.gz")
mewo Mewo.Floor = decodeAsset "mewo/floor" $(embedFile "assets/mewo/floor.bmp.gz")

{- | Nothing for sunny weather: nothing is drawn, matching Python's
blank widget.
-}
weather :: Weather -> Maybe (Image PixelRGBA8)
weather Sunny = Nothing
weather Cloudy = Just (decodeAsset "weather/cloudy" $(embedFile "assets/weather/cloudy.bmp.gz"))
weather Overcast = Just (decodeAsset "weather/overcast" $(embedFile "assets/weather/overcast.bmp.gz"))
weather Rain = Just (decodeAsset "weather/rain" $(embedFile "assets/weather/rain.bmp.gz"))

night :: Image PixelRGBA8
night = decodeAsset "weather/night" $(embedFile "assets/weather/night.bmp.gz")

bert :: BertVariant -> Image PixelRGBA8
bert Bloom = decodeAsset "bert/bloom" $(embedFile "assets/bert/bloom.bmp.gz")
bert Leafless = decodeAsset "bert/leafless" $(embedFile "assets/bert/leafless.bmp.gz")

art :: [Image PixelRGBA8]
art =
    [ decodeAsset "art/lorenz" $(embedFile "assets/art/lorenz.bmp.gz")
    , decodeAsset "art/hedgehog" $(embedFile "assets/art/hedgehog.bmp.gz")
    , decodeAsset "art/love" $(embedFile "assets/art/love.bmp.gz")
    , decodeAsset "art/stones" $(embedFile "assets/art/stones.bmp.gz")
    , decodeAsset "art/system" $(embedFile "assets/art/system.bmp.gz")
    ]

-- Assets are embedded and covered by tests, so a decode failure is a
-- build defect, not a runtime condition worth threading Either through
-- the app for.
decodeAsset :: String -> ByteString -> Image PixelRGBA8
decodeAsset name bytes = case (decodeBitmap . toStrict . decompress . fromStrict) bytes of
    Left err -> error ("asset " <> name <> ": " <> err)
    Right dynamic -> convertRGBA8 dynamic
