module World.Draw where

import Apecs (global)
import Apecs.Gloss (Picture)

import qualified Apecs as Entity

import World.Components (Scene(..))
import World (SystemW)

import qualified Scene.Load as Load
import qualified Scene.Intro as Intro
import qualified Scene.Gameplay as Gameplay
import qualified Scene.Outro as Outro

draw :: SystemW Picture
draw = Entity.get global >>= \case
  Load ->
    Load.draw
  Intro ->
    Intro.draw
  Gameplay ->
    Gameplay.draw
  Outro ->
    Outro.draw
