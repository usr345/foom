module World.Draw where

import Apecs (global)
import Apecs.Gloss (Picture, black, color, foldDraw, rectangleSolid, withAlpha)

import qualified Apecs as Entity

import World.Components (Scene(..), Shade(..))
import World (SystemW)

import qualified Scene.Load as Load
import qualified Scene.Intro as Intro
import qualified Scene.Gameplay as Gameplay
import qualified Scene.Outro as Outro
import Config

draw :: SystemW Picture
draw = do
  scene <- Entity.get global >>= \case
    Load ->
      Load.draw
    Intro ->
      Intro.draw
    Gameplay ->
      Gameplay.draw
    Outro ->
      Outro.draw

  shade <- foldDraw $ \(Shade opaq) ->
    color (withAlpha opaq black) $
      --rectangleSolid 1024 768
      rectangleSolid (windowWidth configuration) (windowHeight configuration)

  pure $ mconcat
    [ scene
    , shade
    ]
