module World.Input where

import Apecs (global)
import Graphics.Gloss.Interface.IO.Interact (Event)

import qualified Apecs as Entity

import Component (Scene(..))
import World (SystemW)

import qualified Scene.Load as Load
import qualified Scene.Intro as Intro
import qualified Scene.Gameplay as Gameplay
import qualified Scene.Outro as Outro

onInput :: Event -> SystemW ()
onInput e = Entity.get global >>= \case
  Load ->
    Load.onInput e
  Intro ->
    Intro.onInput e
  Gameplay ->
    Gameplay.onInput e
  Outro ->
    Outro.onInput e
