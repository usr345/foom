module Scene.Load where

import Apecs (global, ($=))
import Apecs.Gloss (Event, Picture)
import Debug.Trace (traceM)

import Component (Scene(..))
import World (SystemW)

import qualified Scene.Intro

initialize :: SystemW ()
initialize = do
  traceM "Load: initialize"
  global $= Load

draw :: SystemW Picture
draw =
  pure mempty

onTick :: Float -> SystemW ()
onTick _dt =
  Scene.Intro.initialize

onInput :: Event -> SystemW ()
onInput = \case
  _ ->
    pure ()
