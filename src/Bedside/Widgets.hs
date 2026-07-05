{-# LANGUAGE OverloadedStrings #-}

{- | The concrete widgets of the bedside display: who they are, where
they stack, and which artwork they wear.

The layer order (background, then mewo, weather, bert) reproduces the
Python version, where all overlays shared z = -99 and the dict's
insertion order broke the tie.
-}
module Bedside.Widgets (
    artWidgets,
    backgroundWidget,
    mewoWidget,
    weatherWidget,
    nightWidget,
    bertWidget,
)
where

import Bedside.Assets qualified as Assets
import Bedside.Mewo qualified as Mewo
import Bedside.Season (Season, bertVariant)
import Bedside.Weather (Weather)
import Bedside.Widget (Widget (..), canvasHeight, canvasWidth)
import Codec.Picture (Image, PixelRGBA8 (..), generateImage)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

backgroundWidget :: Widget
backgroundWidget = Widget backgroundName (-100) Assets.background

mewoWidget :: Mewo.Pose -> Widget
mewoWidget pose = Widget mewoName (-99) (Assets.mewo pose)

{- | Sunny weather means no overlay; a transparent widget still
replaces whatever weather was showing before.
-}
weatherWidget :: Weather -> Widget
weatherWidget w = Widget weatherName (-98) (fromMaybe transparent (Assets.weather w))

nightWidget :: Widget
nightWidget = Widget weatherName (-98) Assets.night

bertWidget :: Season -> Widget
bertWidget season = Widget bertName (-97) (Assets.bert (bertVariant season))

backgroundName, mewoName, weatherName, bertName :: Text
backgroundName = "background"
mewoName = "mewo"
weatherName = "weather"
bertName = "bert"

artWidgets :: [Widget]
artWidgets = map (Widget "Art" (-99)) Assets.art

transparent :: Image PixelRGBA8
transparent = generateImage (\_ _ -> PixelRGBA8 255 255 255 0) canvasWidth canvasHeight
