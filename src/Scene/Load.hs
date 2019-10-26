module Scene.Load where

import Apecs (global, ($=))
import Apecs.Gloss (Event, Picture)
import Debug.Trace (traceM)

import World.Components (Scene(..), Time(..))
import World (SystemW)

import qualified Scene.Intro

initialize :: SystemW ()
initialize = do
  traceM "Load: initialize"
  global $= (Load, Time 0)
  -- TODO: load sounds etc.

draw :: SystemW Picture
draw =
  -- TODO: render splash screen and maybe progress bar.
  pure mempty

onTick :: Float -> SystemW ()
onTick _dt =
  Scene.Intro.initialize

onInput :: Event -> SystemW ()
onInput = \case
  _ ->
    pure ()
