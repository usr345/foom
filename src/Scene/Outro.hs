module Scene.Outro where

import Debug.Trace (traceM)
import System.Exit (exitSuccess)

import Apecs (global, liftIO, ($=))
import Apecs.Gloss (Event, Picture)

import World.Components (Scene(..), Time(..))
import World (SystemW)

initialize :: SystemW ()
initialize = do
  traceM "Outro: initialize"
  global $= (Outro, Time 0)

draw :: SystemW Picture
draw =
  -- TODO: fade out or something
  pure mempty

onTick :: Float -> SystemW ()
onTick _dt = do
  traceM "Outro: exit"
  liftIO exitSuccess

onInput :: Event -> SystemW ()
onInput = \case
  _ ->
    pure ()
