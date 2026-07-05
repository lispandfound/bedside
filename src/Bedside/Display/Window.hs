-- | Dev backend: an SDL2 window that shows each frame the way the
-- panel would. Built only when the @gui@ cabal flag is on.
module Bedside.Display.Window
  ( openWindow,
  )
where

import Bedside.Display (Display (..))
import Bedside.Frame (Frame (..), preview)
import Codec.Picture (Image (..))
import Control.Concurrent (ThreadId, forkOS, myThreadId, threadDelay, throwTo)
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr (castForeignPtr)
import SDL qualified
import System.Exit (ExitCode (ExitSuccess))

-- | Open the preview window. The SDL loop runs on its own OS thread;
-- 'refresh' just hands it the latest frame. Closing the window exits
-- the program.
openWindow :: IO Display
openWindow = do
  frameVar <- newTVarIO Nothing
  quitVar <- newTVarIO False
  doneVar <- newTVarIO False
  caller <- myThreadId
  _ <- forkOS (windowLoop caller frameVar quitVar doneVar)
  pure
    Display
      { refresh = atomically . writeTVar frameVar . Just,
        shutdown = atomically $ do
          writeTVar quitVar True
          done <- readTVar doneVar
          unless done retry
      }

windowLoop :: ThreadId -> TVar (Maybe Frame) -> TVar Bool -> TVar Bool -> IO ()
windowLoop caller frameVar quitVar doneVar = do
  SDL.initialize [SDL.InitVideo]
  window <-
    SDL.createWindow
      (T.pack "bedside")
      SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 800 480}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  texture <-
    SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessStreaming (SDL.V2 800 480)

  let loop shown = do
        events <- SDL.pollEvents
        let closed = any ((== SDL.QuitEvent) . SDL.eventPayload) events
        when closed (throwTo caller ExitSuccess)

        newFrame <- atomically $ do
          frame <- readTVar frameVar
          writeTVar frameVar Nothing
          pure frame
        shown' <- case newFrame of
          Nothing -> pure shown
          Just frame -> do
            _ <- SDL.updateTexture texture Nothing (frameBytes frame) (800 * 3)
            pure True
        when shown' $ do
          SDL.copy renderer texture Nothing Nothing
          SDL.present renderer

        quit <- readTVarIO quitVar
        unless quit $ do
          threadDelay 50_000
          loop shown'

  loop False
  SDL.destroyTexture texture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
  atomically (writeTVar doneVar True)

-- | Panel-style RGB rendering of the frame, as raw RGB24 bytes.
frameBytes :: Frame -> BS.ByteString
frameBytes frame = BSI.fromForeignPtr (castForeignPtr fp) 0 len
  where
    pixels = imageData (preview frame)
    (fp, len) = VS.unsafeToForeignPtr0 pixels
