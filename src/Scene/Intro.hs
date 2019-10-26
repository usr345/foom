module Scene.Intro
  ( initialize
  , draw
  , onInput
  , onTick
  ) where

import Debug.Trace (traceM)

import Apecs (global, ($=))
import Apecs.Gloss (Event(..), Key(..), KeyState(..), SpecialKey(..))

-- import qualified Apecs as Entity

import World.Components (Scene(..), Time(..))
import World (SystemW)

import Scene.Intro.Draw (draw)

import qualified Scene.Gameplay
import qualified Utils.Debug as Debug

initialize :: SystemW ()
initialize = do
  traceM "Intro: initialize"
  Debug.newMeasure_
  global $= (Intro, Time 0)

onTick :: Float -> SystemW ()
onTick _dt =
  pure ()

onInput :: Event -> SystemW ()
onInput = \case
  EventKey key Down _mods _pos ->
    case key of
      SpecialKey KeyEsc ->
        -- XXX: prevent flash of gameplay before quit
        pure ()
      SpecialKey KeySpace ->
        Scene.Gameplay.initialize
      _ ->
        Debug.measureOnKey key
  _ ->
    pure ()
