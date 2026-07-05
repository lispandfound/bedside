-- | Named image layers and z-ordered compositing.
module Bedside.Widget
  ( Widget (..),
    canvasWidth,
    canvasHeight,
    composite,
  )
where

import Codec.Picture
import Data.List (sortOn)
import Data.Text (Text)

canvasWidth, canvasHeight :: Int
canvasWidth = 800
canvasHeight = 480

-- | A named layer. The app keeps one widget per name; z orders
-- compositing, lower z drawn first.
data Widget = Widget
  { widgetName :: Text,
    widgetZ :: Int,
    widgetImage :: Image PixelRGBA8
  }

-- | Composite widgets over an opaque white canvas in ascending z order.
-- Images smaller than the canvas are anchored at the top-left; pixels
-- outside an image are treated as transparent.
composite :: [Widget] -> Image PixelRGBA8
composite widgets = generateImage pixel canvasWidth canvasHeight
  where
    layers = map widgetImage (sortOn widgetZ widgets)
    white = PixelRGBA8 255 255 255 255
    pixel x y = foldl' (\dst img -> sample img x y `over` dst) white layers
    sample img x y
      | x < imageWidth img && y < imageHeight img = pixelAt img x y
      | otherwise = PixelRGBA8 0 0 0 0

-- | Source-over blending of straight (non-premultiplied) pixels.
over :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
over (PixelRGBA8 sr sg sb sa) (PixelRGBA8 dr dg db da) =
  PixelRGBA8 (channel sr dr) (channel sg dg) (channel sb db) alphaOut
  where
    i = fromIntegral :: (Integral a) => a -> Int
    -- 255^2 times the resulting alpha fraction
    aDen = i sa * 255 + i da * (255 - i sa)
    alphaOut = fromIntegral (aDen `div` 255)
    channel s d
      | aDen == 0 = 0
      | otherwise = fromIntegral ((i s * i sa * 255 + i d * i da * (255 - i sa)) `div` aDen)
