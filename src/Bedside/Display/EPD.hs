-- | The real panel: binds the EPD protocol to spidev and the GPIO
-- character device on the Pi.
module Bedside.Display.EPD
  ( Pins (..),
    defaultPins,
    openEpd,
    openEpdWith,
  )
where

import Bedside.Display (Display (..))
import Bedside.EPD.Protocol
import Bedside.Frame (Frame (..))
import Bedside.Hardware.Gpio qualified as Gpio
import Bedside.Hardware.Spi qualified as Spi
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Word (Word32)

-- | BCM pin numbers for the WaveShare e-paper HAT. Chip select is
-- spidev's hardware CE0, so it does not appear here.
data Pins = Pins
  { rstPin :: Word32,
    dcPin :: Word32,
    pwrPin :: Word32,
    busyPin :: Word32
  }

defaultPins :: Pins
defaultPins = Pins {rstPin = 17, dcPin = 25, pwrPin = 18, busyPin = 24}

openEpd :: IO Display
openEpd = openEpdWith defaultPins "/dev/spidev0.0" "/dev/gpiochip0"

openEpdWith :: Pins -> FilePath -> FilePath -> IO Display
openEpdWith pins spiDev gpioDev = do
  spi <- Spi.openSpi spiDev 4_000_000
  chip <- Gpio.openChip gpioDev
  rst <- Gpio.requestOutput chip (rstPin pins)
  dc <- Gpio.requestOutput chip (dcPin pins)
  pwr <- Gpio.requestOutput chip (pwrPin pins)
  -- BUSY idles high on this panel; low means a refresh is in progress.
  busy <- Gpio.requestInput chip (busyPin pins) Gpio.PullDown
  Gpio.setLine pwr True

  let io =
        EpdIo
          { ioCommand = \c -> do
              Gpio.setLine dc False
              Spi.writeSpi spi (BS.singleton c),
            ioSend = \bytes -> do
              Gpio.setLine dc True
              Spi.writeSpi spi bytes,
            ioSetReset = Gpio.setLine rst,
            ioReadBusy = Gpio.getLine busy,
            ioDelayMs = \ms -> threadDelay (ms * 1000)
          }

  pure
    Display
      { -- Full init/clear/refresh/deep-sleep cycle per frame, as the
        -- Python version did: the panel spends its life asleep and is
        -- woken (via Reset in the init sequence) for each update.
        refresh = \frame -> do
          checkFrame frame
          runCmds io initSequence
          runCmds io clearSequence
          runCmds io (displaySequence frame)
          runCmds io sleepSequence,
        shutdown = do
          mapM_ (`Gpio.setLine` False) [rst, dc, pwr]
          mapM_ Gpio.closeLine [rst, dc, pwr, busy]
          Gpio.closeChip chip
          Spi.closeSpi spi
      }

checkFrame :: Frame -> IO ()
checkFrame frame =
  unless (BS.length (blackPlane frame) == planeBytes && BS.length (redPlane frame) == planeBytes) $
    ioError (userError "refresh: frame is not panel-sized")
