-- | The seam between the app and whatever is showing pixels.
module Bedside.Display
  ( Display (..),
  )
where

import Bedside.Frame (Frame)

-- | A place frames can be shown. The app only ever pushes a full frame
-- and eventually shuts the device down; everything else (panel init,
-- clearing, deep sleep) is the backend's business.
data Display = Display
  { refresh :: Frame -> IO (),
    shutdown :: IO ()
  }
