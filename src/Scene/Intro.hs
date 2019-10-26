module Scene.Intro
  ( initialize
  , draw
  , onInput
  , onTick
  ) where

import Control.Lens hiding (set)
import Debug.Trace (traceM)

import Apecs (cmap, global, newEntity, ($=))
import Apecs.Gloss (Event(..), Key(..), KeyState(..), SpecialKey(..))

-- import qualified Apecs as Entity

import Scene.Intro.Components
import World.Components (Scene(..), Time(..))
import World (SystemW)

import Scene.Intro.Draw (draw)

import qualified Scene.Gameplay

initialize :: SystemW ()
initialize = do
  traceM "Intro: initialize"

  _ <- newEntity IntroState
    { _isOffset = 0
    , _isScaleX = 0.16
    , _isScaleY = 0.12
    }

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
      Char 'q' ->
        cmap $ isOffset +~ -2
      Char 'e' ->
        cmap $ isOffset +~ 2
      Char 'w' ->
        cmap $ isScaleY +~ 0.01
      Char 's' ->
        cmap $ isScaleY -~ 0.01
      Char 'a' ->
        cmap $ isScaleX -~ 0.01
      Char 'd' ->
        cmap $ isScaleX +~ 0.01
      _ ->
        Scene.Gameplay.initialize
  _ ->
    pure ()
