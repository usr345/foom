module Scene.Intro where

import Apecs (global, ($=))
import Apecs.Gloss (Event, Picture)
import Debug.Trace (traceM)

import Component (Scene(..))
import World (SystemW)

import qualified Scene.Gameplay

initialize :: SystemW ()
initialize = do
  traceM "Intro: initialize"
  global $= Intro

draw :: SystemW Picture
draw =
  pure mempty

onTick :: Float -> SystemW ()
onTick _dt =
  Scene.Gameplay.initialize

onInput :: Event -> SystemW ()
onInput = \case
  _ ->
    pure ()
