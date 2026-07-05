{-# LANGUAGE CApiFFI #-}

-- | GPIO via the Linux v2 character-device interface
-- (@\/dev\/gpiochipN@ + ioctls). One requested line per handle, which
-- is all the four panel pins need. No libgpiod dependency.
module Bedside.Hardware.Gpio
  ( GpioChip,
    Line,
    Bias (..),
    openChip,
    closeChip,
    requestOutput,
    requestInput,
    setLine,
    getLine,
    closeLine,
  )
where

#include <linux/gpio.h>

import Prelude hiding (getLine)

import Control.Monad (void)
import Data.Bits ((.|.), testBit)
import Data.Word (Word32, Word64)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.String (withCStringLen)
import Foreign.C.Types (CChar, CInt (..), CULong (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd (..))

foreign import capi unsafe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

newtype GpioChip = GpioChip Fd

-- | A single requested GPIO line.
newtype Line = Line Fd

data Bias = NoBias | PullUp | PullDown

openChip :: FilePath -> IO GpioChip
openChip path = GpioChip <$> openFd path ReadWrite defaultFileFlags

closeChip :: GpioChip -> IO ()
closeChip (GpioChip fd) = closeFd fd

requestOutput :: GpioChip -> Word32 -> IO Line
requestOutput chip pin = requestLine chip pin #{const GPIO_V2_LINE_FLAG_OUTPUT}

requestInput :: GpioChip -> Word32 -> Bias -> IO Line
requestInput chip pin bias =
  requestLine chip pin (#{const GPIO_V2_LINE_FLAG_INPUT} .|. biasFlag)
  where
    biasFlag = case bias of
      NoBias -> 0
      PullUp -> #{const GPIO_V2_LINE_FLAG_BIAS_PULL_UP}
      PullDown -> #{const GPIO_V2_LINE_FLAG_BIAS_PULL_DOWN}

requestLine :: GpioChip -> Word32 -> Word64 -> IO Line
requestLine (GpioChip (Fd chipFd)) pin flags =
  allocaBytes #{size struct gpio_v2_line_request} $ \req -> do
    fillBytes req 0 #{size struct gpio_v2_line_request}
    -- offsets[0] = pin
    #{poke struct gpio_v2_line_request, offsets} req pin
    #{poke struct gpio_v2_line_request, num_lines} req (1 :: Word32)
    pokeConsumer req "bedside"
    let config = req `plusPtr` #{offset struct gpio_v2_line_request, config}
    #{poke struct gpio_v2_line_config, flags} config flags
    void (throwErrnoIfMinus1 "gpio get line" (c_ioctl chipFd #{const GPIO_V2_GET_LINE_IOCTL} req))
    lineFd <- #{peek struct gpio_v2_line_request, fd} req :: IO CInt
    pure (Line (Fd lineFd))

-- | Label the line in gpioinfo output. The consumer field is a
-- fixed-size char array; the request was zeroed, so any prefix write
-- stays NUL-terminated.
pokeConsumer :: Ptr a -> String -> IO ()
pokeConsumer req name =
  withCStringLen name $ \(str, len) ->
    copyBytes
      (req `plusPtr` #{offset struct gpio_v2_line_request, consumer} :: Ptr CChar)
      str
      (min len (#{const GPIO_MAX_NAME_SIZE} - 1))

setLine :: Line -> Bool -> IO ()
setLine (Line (Fd fd)) value =
  withLineValues (if value then 1 else 0) $ \vals ->
    void (throwErrnoIfMinus1 "gpio set" (c_ioctl fd #{const GPIO_V2_LINE_SET_VALUES_IOCTL} vals))

getLine :: Line -> IO Bool
getLine (Line (Fd fd)) =
  withLineValues 0 $ \vals -> do
    void (throwErrnoIfMinus1 "gpio get" (c_ioctl fd #{const GPIO_V2_LINE_GET_VALUES_IOCTL} vals))
    bits <- #{peek struct gpio_v2_line_values, bits} vals :: IO Word64
    pure (testBit bits 0)

withLineValues :: Word64 -> (Ptr a -> IO b) -> IO b
withLineValues bits action =
  allocaBytes #{size struct gpio_v2_line_values} $ \vals -> do
    #{poke struct gpio_v2_line_values, bits} vals bits
    #{poke struct gpio_v2_line_values, mask} vals (1 :: Word64)
    action (castPtr vals)

closeLine :: Line -> IO ()
closeLine (Line fd) = closeFd fd
