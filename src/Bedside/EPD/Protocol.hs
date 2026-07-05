-- | The UC8179 command protocol for the WaveShare 7.5" B\/W\/Red panel
-- (epd7in5b V2), expressed as data plus a small interpreter.
--
-- This replaces the vendored WaveShare driver. Only the operations the
-- app uses survive: hardware reset, busy-wait, init, clear, full
-- refresh, deep sleep. The interpreter runs over an abstract 'EpdIo'
-- handle so tests can record the byte traffic without hardware.
--
-- Wire format for image planes (see 'Bedside.Frame.Frame'):
--   black RAM (0x10): bit 1 = white, bit 0 = black
--   red RAM   (0x13): bit 1 = red (red overrides black on the panel)
module Bedside.EPD.Protocol
  ( -- * Commands as data
    Cmd (..),
    initSequence,
    clearSequence,
    displaySequence,
    sleepSequence,
    planeBytes,

    -- * Interpretation
    EpdIo (..),
    runCmds,

    -- * Register names
    panelSetting,
    powerSetting,
    powerOff,
    powerOn,
    boosterSoftStart,
    deepSleep,
    txBlack,
    txRed,
    displayRefresh,
    dualSpi,
    vcomInterval,
    tconSetting,
    resolutionSetting,
    getStatus,
  )
where

import Bedside.Frame (Frame (..))
import Bedside.Widget (canvasHeight, canvasWidth)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Word (Word8)

data Cmd
  = Command Word8
  | Send BS.ByteString
  | DelayMs Int
  | Reset
  | WaitBusy
  deriving (Eq, Show)

-- UC8179 register addresses
panelSetting, powerSetting, powerOff, powerOn, boosterSoftStart, deepSleep :: Word8
txBlack, txRed, displayRefresh, dualSpi, vcomInterval, tconSetting :: Word8
resolutionSetting, getStatus :: Word8
panelSetting = 0x00
powerSetting = 0x01
powerOff = 0x02
powerOn = 0x04
boosterSoftStart = 0x06
deepSleep = 0x07
txBlack = 0x10
displayRefresh = 0x12
txRed = 0x13
dualSpi = 0x15
vcomInterval = 0x50
tconSetting = 0x60
resolutionSetting = 0x61
getStatus = 0x71

-- | Bytes per image plane: one bit per pixel.
planeBytes :: Int
planeBytes = canvasWidth `div` 8 * canvasHeight

set :: Word8 -> [Word8] -> [Cmd]
set reg payload = [Command reg, Send (BS.pack payload)]

-- | Power-on and panel configuration, transcribed from
-- @epd7in5b_V2.EPD.init@.
initSequence :: [Cmd]
initSequence =
  concat
    [ [Reset],
      set powerSetting [0x07, 0x07, 0x3F, 0x3F],
      set boosterSoftStart [0x17, 0x17, 0x28, 0x17],
      [Command powerOn, DelayMs 100, WaitBusy],
      set panelSetting [0x0F],
      set resolutionSetting [0x03, 0x20, 0x01, 0xE0], -- 800 x 480
      set dualSpi [0x00],
      set vcomInterval [0x11, 0x07],
      set tconSetting [0x22]
    ]

-- | Blank both RAMs and refresh.
clearSequence :: [Cmd]
clearSequence =
  displayPlanes
    (BS.replicate planeBytes 0xFF)
    (BS.replicate planeBytes 0x00)

-- | Write both planes and refresh.
displaySequence :: Frame -> [Cmd]
displaySequence frame = displayPlanes (blackPlane frame) (redPlane frame)

displayPlanes :: BS.ByteString -> BS.ByteString -> [Cmd]
displayPlanes black red =
  [ Command txBlack,
    Send black,
    Command txRed,
    Send red,
    Command displayRefresh,
    DelayMs 100,
    WaitBusy
  ]

-- | Power down into deep sleep; only a hardware reset wakes the panel.
sleepSequence :: [Cmd]
sleepSequence =
  concat
    [ [Command powerOff, WaitBusy],
      set deepSleep [0xA5],
      [DelayMs 2000]
    ]

-- | What the panel is attached to. 'Bedside.Display.EPD' provides the
-- real SPI\/GPIO binding; tests provide a recording fake.
data EpdIo = EpdIo
  { -- | Send a command byte (DC low on hardware).
    ioCommand :: Word8 -> IO (),
    -- | Send data bytes (DC high on hardware).
    ioSend :: BS.ByteString -> IO (),
    -- | Drive the RST line.
    ioSetReset :: Bool -> IO (),
    -- | Sample the BUSY line; True = panel idle.
    ioReadBusy :: IO Bool,
    ioDelayMs :: Int -> IO ()
  }

runCmds :: EpdIo -> [Cmd] -> IO ()
runCmds io = mapM_ run
  where
    run (Command c) = ioCommand io c
    run (Send bytes) = ioSend io bytes
    run (DelayMs ms) = ioDelayMs io ms
    run Reset = do
      ioSetReset io True
      ioDelayMs io 200
      ioSetReset io False
      ioDelayMs io 4
      ioSetReset io True
      ioDelayMs io 200
    run WaitBusy = do
      -- The panel reports status only after a getStatus command; poll
      -- until the BUSY line releases, then let it settle.
      ioCommand io getStatus
      poll
      ioDelayMs io 200
    poll = do
      idle <- ioReadBusy io
      unless idle $ do
        ioDelayMs io 10
        ioCommand io getStatus
        poll
