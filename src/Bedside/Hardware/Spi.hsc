{-# LANGUAGE CApiFFI #-}

-- | Minimal spidev access: open the device, set mode 0 and a max
-- clock, write bytes. Transfers are half-duplex writes, which is all
-- the panel needs.
module Bedside.Hardware.Spi
  ( Spi,
    openSpi,
    writeSpi,
    closeSpi,
  )
where

#include <linux/spi/spidev.h>

import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS (unsafeUseAsCStringLen)
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, poke)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt (..), CULong (..))
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, fdWriteBuf, openFd)
import System.Posix.Types (Fd (..))

foreign import capi unsafe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

newtype Spi = Spi Fd

-- | spidev's default transfer buffer size; larger writes fail with
-- EMSGSIZE, so 'writeSpi' chunks to this.
maxChunk :: Int
maxChunk = 4096

openSpi :: FilePath -> Int -> IO Spi
openSpi path speedHz = do
  fd <- openFd path ReadWrite defaultFileFlags
  setAttr fd #{const SPI_IOC_WR_MODE} (0 :: Word8)
  setAttr fd #{const SPI_IOC_WR_MAX_SPEED_HZ} (fromIntegral speedHz :: Word32)
  pure (Spi fd)

setAttr :: Storable a => Fd -> CULong -> a -> IO ()
setAttr (Fd fd) request value =
  alloca $ \ptr -> do
    poke ptr value
    void (throwErrnoIfMinus1 "spi ioctl" (c_ioctl fd request ptr))

writeSpi :: Spi -> ByteString -> IO ()
writeSpi spi@(Spi fd) bytes =
  unless (BS.null bytes) $ do
    let (chunk, rest) = BS.splitAt maxChunk bytes
    written <- BS.unsafeUseAsCStringLen chunk $ \(ptr, len) ->
      fdWriteBuf fd (castPtr ptr) (fromIntegral len)
    -- spidev transfers a chunk whole or not at all
    unless (fromIntegral written == BS.length chunk) $
      ioError (userError "spi write: short write")
    writeSpi spi rest

closeSpi :: Spi -> IO ()
closeSpi (Spi fd) = closeFd fd
