module ProtocolTests (tests) where

import Bedside.EPD.Protocol
import Bedside.Frame (Frame (..))
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

-- | Everything the interpreter can do to the hardware, in order.
data Event
  = Cmd Word8
  | Dat BS.ByteString
  | Rst Bool
  | Wait Int
  deriving (Eq, Show)

-- | An 'EpdIo' that records events. The busy line reports busy for the
-- given number of polls before releasing.
recordingIo :: Int -> IO (EpdIo, IO [Event])
recordingIo busyPolls = do
  events <- newIORef []
  busy <- newIORef busyPolls
  let record e = modifyIORef' events (e :)
      io =
        EpdIo
          { ioCommand = record . Cmd,
            ioSend = record . Dat,
            ioSetReset = record . Rst,
            ioDelayMs = record . Wait,
            ioReadBusy = do
              remaining <- readIORef busy
              writeIORef busy (remaining - 1)
              pure (remaining <= 0)
          }
  pure (io, reverse <$> readIORef events)

runRecorded :: Int -> [Cmd] -> IO [Event]
runRecorded busyPolls cmds = do
  (io, getEvents) <- recordingIo busyPolls
  runCmds io cmds
  getEvents

tests :: TestTree
tests =
  testGroup
    "EPD protocol"
    [ testCase "init sequence byte trace" $ do
        events <- runRecorded 0 initSequence
        events
          @?= [ Rst True,
                Wait 200,
                Rst False,
                Wait 4,
                Rst True,
                Wait 200,
                Cmd 0x01,
                Dat (BS.pack [0x07, 0x07, 0x3F, 0x3F]),
                Cmd 0x06,
                Dat (BS.pack [0x17, 0x17, 0x28, 0x17]),
                Cmd 0x04,
                Wait 100,
                Cmd 0x71,
                Wait 200,
                Cmd 0x00,
                Dat (BS.pack [0x0F]),
                Cmd 0x61,
                Dat (BS.pack [0x03, 0x20, 0x01, 0xE0]),
                Cmd 0x15,
                Dat (BS.pack [0x00]),
                Cmd 0x50,
                Dat (BS.pack [0x11, 0x07]),
                Cmd 0x60,
                Dat (BS.pack [0x22])
              ],
      testCase "busy line is polled until it releases" $ do
        events <- runRecorded 3 [WaitBusy]
        events
          @?= [ Cmd 0x71,
                Wait 10,
                Cmd 0x71,
                Wait 10,
                Cmd 0x71,
                Wait 10,
                Cmd 0x71,
                Wait 200
              ],
      testCase "clear writes full white/no-red planes" $ do
        events <- runRecorded 0 clearSequence
        events
          @?= [ Cmd 0x10,
                Dat (BS.replicate planeBytes 0xFF),
                Cmd 0x13,
                Dat (BS.replicate planeBytes 0x00),
                Cmd 0x12,
                Wait 100,
                Cmd 0x71,
                Wait 200
              ],
      testCase "display sends the frame planes untouched" $ do
        let frame = Frame 800 480 (BS.replicate planeBytes 0xAB) (BS.replicate planeBytes 0xCD)
        events <- runRecorded 0 (displaySequence frame)
        take 4 events
          @?= [ Cmd 0x10,
                Dat (BS.replicate planeBytes 0xAB),
                Cmd 0x13,
                Dat (BS.replicate planeBytes 0xCD)
              ],
      testCase "sleep powers off then enters deep sleep" $ do
        events <- runRecorded 0 sleepSequence
        events
          @?= [ Cmd 0x02,
                Cmd 0x71,
                Wait 200,
                Cmd 0x07,
                Dat (BS.pack [0xA5]),
                Wait 2000
              ]
    ]
