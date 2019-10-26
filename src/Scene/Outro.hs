module Scene.Outro where

import Debug.Trace (traceM)
import System.Exit (exitSuccess)

import Apecs (global, liftIO, ($=))
import Apecs.Gloss (Event, Picture)

import Component (Scene(..))
import World (SystemW)

initialize :: SystemW ()
initialize = do
  traceM "Outro: initialize"
  global $= Outro

draw :: SystemW Picture
draw =
  pure mempty

onTick :: Float -> SystemW ()
onTick _dt = do
  traceM "Outro: exit"
  liftIO exitSuccess

onInput :: Event -> SystemW ()
onInput = \case
  _ ->
    pure ()
