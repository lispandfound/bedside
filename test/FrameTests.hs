module FrameTests (tests) where

import Bedside.Frame
import Bedside.Widget
import Codec.Picture
import Codec.Picture.Types (promoteImage)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

red, black, white, transparent :: PixelRGBA8
red = PixelRGBA8 255 0 0 255
black = PixelRGBA8 0 0 0 255
white = PixelRGBA8 255 255 255 255
transparent = PixelRGBA8 0 0 0 0

tests :: TestTree
tests =
  testGroup
    "Frame"
    [ testGroup
        "classify"
        [ testCase "opaque red is Red" $ classify red @?= Red,
          testCase "red with blue tint is still Red" $ classify (PixelRGBA8 255 0 200 255) @?= Red,
          testCase "translucent red is Black (dark, not panel red)" $ classify (PixelRGBA8 255 0 0 254) @?= Black,
          testCase "black is Black" $ classify black @?= Black,
          testCase "white is White" $ classify white @?= White,
          testCase "mid grey at threshold is White" $ classify (PixelRGBA8 128 128 128 255) @?= White,
          testCase "dark grey is Black" $ classify (PixelRGBA8 100 100 100 255) @?= Black
        ],
      testGroup
        "bit packing"
        [ testCase "black row packs to 0x00 / red bit clear" $ do
            let frame = toFrame (rowImage (replicate 8 black))
            blackPlane frame @?= BS.pack [0x00]
            redPlane frame @?= BS.pack [0x00],
          testCase "red row sets red bits, leaves black plane white" $ do
            let frame = toFrame (rowImage (replicate 8 red))
            blackPlane frame @?= BS.pack [0xFF]
            redPlane frame @?= BS.pack [0xFF],
          testCase "white row is all ones black plane, no red" $ do
            let frame = toFrame (rowImage (replicate 8 white))
            blackPlane frame @?= BS.pack [0xFF]
            redPlane frame @?= BS.pack [0x00],
          testCase "bits are MSB-first" $ do
            -- black in leftmost pixel only -> top bit cleared
            let frame = toFrame (rowImage (black : replicate 7 white))
            blackPlane frame @?= BS.pack [0x7F]
        ],
      testGroup
        "preview round-trip"
        [ testCase "toFrame . promote . preview = id on a mixed pattern" $ do
            let img = generateImage pattern 16 4
                pattern x y
                  | (x + y) `mod` 3 == 0 = red
                  | (x + y) `mod` 3 == 1 = black
                  | otherwise = white
                frame = toFrame img
            toFrame (promoteImage (preview frame)) @?= frame
        ],
      testGroup
        "composite"
        [ testCase "higher z wins" $ do
            let bottom = layer "bottom" 0 black
                top = layer "top" 1 red
                img = composite [top, bottom]
            classify (pixelAt img 0 0) @?= Red,
          testCase "transparent layer does not cover" $ do
            let bottom = layer "bottom" 0 black
                top = layer "top" 1 transparent
                img = composite [top, bottom]
            classify (pixelAt img 0 0) @?= Black,
          testCase "empty composite is white canvas" $ do
            let img = composite []
            imageWidth img @?= canvasWidth
            imageHeight img @?= canvasHeight
            classify (pixelAt img 400 200) @?= White,
          testCase "small layers only cover their own extent" $ do
            let small = Widget (T.pack "small") 0 (generateImage (\_ _ -> black) 8 8)
                img = composite [small]
            classify (pixelAt img 0 0) @?= Black
            classify (pixelAt img 100 100) @?= White
        ]
    ]

-- | A single-row image from a list of pixels.
rowImage :: [PixelRGBA8] -> Image PixelRGBA8
rowImage pixels = generateImage (\x _ -> pixels !! x) (length pixels) 1

-- | A full-canvas layer of one solid colour.
layer :: String -> Int -> PixelRGBA8 -> Widget
layer name z colour = Widget (T.pack name) z (generateImage (\_ _ -> colour) canvasWidth canvasHeight)
