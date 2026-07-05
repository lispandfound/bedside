-- | Conversion from a composited RGBA image to the two 1-bit planes the
-- panel understands, and back to an RGB preview for the window backend
-- and tests.
module Bedside.Frame
  ( Frame (..),
    Ink (..),
    classify,
    toFrame,
    preview,
  )
where

import Codec.Picture
import Data.Bits (testBit)
import Data.ByteString qualified as BS
import Data.Word (Word8)

-- | One panel refresh. Planes are row-major, MSB-first, width/8 bytes
-- per row (48000 bytes at full 800x480).
--
-- Panel wire conventions (UC8179):
--   black plane: bit 1 = white, bit 0 = black
--   red plane:   bit 1 = red,  bit 0 = no red (red overrides black)
data Frame = Frame
  { frameWidth :: Int,
    frameHeight :: Int,
    blackPlane :: BS.ByteString,
    redPlane :: BS.ByteString
  }
  deriving (Eq, Show)

data Ink = White | Black | Red
  deriving (Eq, Show)

-- | Panel ink for a composited pixel. Red is exactly the opaque
-- "red channel on, green off" colour the assets use; otherwise dark
-- pixels print black and everything else stays white.
classify :: PixelRGBA8 -> Ink
classify (PixelRGBA8 r g b a)
  | a == 255 && r == 255 && g == 0 = Red
  | luminance < 128 = Black
  | otherwise = White
  where
    luminance =
      (299 * fromIntegral r + 587 * fromIntegral g + 114 * fromIntegral b) `div` 1000 :: Int

-- | Classify and bit-pack a composited image. The width must be a
-- multiple of 8 (the canvas is).
toFrame :: Image PixelRGBA8 -> Frame
toFrame img
  | w `mod` 8 /= 0 = error ("toFrame: width not a multiple of 8: " <> show w)
  | otherwise =
      Frame
        { frameWidth = w,
          frameHeight = h,
          blackPlane = plane (/= Black),
          redPlane = plane (== Red)
        }
  where
    w = imageWidth img
    h = imageHeight img
    inkAt x y = classify (pixelAt img x y)
    plane bit =
      BS.pack
        [ packByte [bit (inkAt (x0 + i) y) | i <- [0 .. 7]]
        | y <- [0 .. h - 1],
          x0 <- [0, 8 .. w - 1]
        ]

packByte :: [Bool] -> Word8
packByte = foldl (\acc b -> acc * 2 + (if b then 1 else 0)) 0

-- | Render a frame the way the panel would show it.
preview :: Frame -> Image PixelRGB8
preview (Frame w h black red) = generateImage pixel w h
  where
    stride = w `div` 8
    bitAt plane x y = testBit (BS.index plane (y * stride + x `div` 8)) (7 - x `mod` 8)
    pixel x y
      | bitAt red x y = PixelRGB8 255 0 0
      | not (bitAt black x y) = PixelRGB8 0 0 0
      | otherwise = PixelRGB8 255 255 255
