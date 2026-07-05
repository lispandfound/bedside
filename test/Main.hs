module Main (main) where

import Test.Tasty

import ConfigTests qualified
import FrameTests qualified
import PortTests qualified
import ProtocolTests qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "bedside"
      [ FrameTests.tests,
        ProtocolTests.tests,
        PortTests.tests,
        ConfigTests.tests
      ]
