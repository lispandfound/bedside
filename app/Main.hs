{-# LANGUAGE CPP #-}

module Main (main) where

import Bedside.App qualified as App
import Bedside.Config (loadConfig)
import Bedside.Display (Display (..))
import Bedside.Display.EPD (openEpd)
import Bedside.Log (logInfo)
import Control.Exception (finally)
import Options.Applicative
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)

#ifdef GUI
import Bedside.Display.Window (openWindow)
#else
import System.Exit (die)
#endif

data Backend = Epd | Window

data Options = Options
  { configPath :: FilePath,
    backend :: Backend
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "config"
          <> metavar "PATH"
          <> help "Path to the JSON config file (see config.example.json)"
      )
    <*> option
      (eitherReader readBackend)
      ( long "display"
          <> metavar "epd|window"
          <> value Epd
          <> help "Where to render: the e-paper panel (default) or an SDL window"
      )
  where
    readBackend "epd" = Right Epd
    readBackend "window" = Right Window
    readBackend other = Left ("unknown display: " <> other)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  opts <-
    execParser $
      info
        (options <**> helper)
        (fullDesc <> progDesc "Bedside room display")

  config <- loadConfig (configPath opts)

  display <- case backend opts of
    Epd -> openEpd
#ifdef GUI
    Window -> openWindow
#else
    Window -> die "this build has no window backend (rebuild with -fgui)"
#endif

  logInfo "starting bedside"
  App.run display config
    `finally` (logInfo "shutting down display" >> shutdown display)
