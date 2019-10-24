module System.Init where

import Control.Monad (void)

import Apecs (global, newEntity, ($=))

import Component
import World (SystemW)

import qualified Scene.Gameplay as Game

initialize :: SystemW ()
initialize = do
  void $ newEntity (Window 0 0)
  void $ newEntity Cursor

  Game.initialize

  global $= Gameplay -- TODO: intro
