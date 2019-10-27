module World.Input where

import Control.Lens hiding (set)

import Apecs (cmap, global)
import Apecs.Gloss (Camera(..), Event(..))

import qualified Apecs as Entity

import World.Components (Scene(..), screenWidth, screenHeight)
import World (SystemW)

import qualified Scene.Load as Load
import qualified Scene.Intro as Intro
import qualified Scene.Gameplay as Gameplay
import qualified Scene.Outro as Outro

onInput :: Event -> SystemW ()
onInput = \case
  EventResize (newW, newH) -> do
    cmap $ \w -> w
      & screenWidth .~ newW
      & screenHeight .~ newH

    Entity.modify global $ \cam -> cam
      { camScale =
          if newW >= newH then
            fromIntegral newH / 768
          else
            fromIntegral newW / 1024
      }
  e ->
    Entity.get global >>= \case
      Load ->
        Load.onInput e
      Intro ->
        Intro.onInput e
      Gameplay ->
        Gameplay.onInput e
      Outro ->
        Outro.onInput e
