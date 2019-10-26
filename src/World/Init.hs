module World.Init where

import Control.Monad (void)

import Apecs (newEntity)

import World.Components
import World (SystemW)

import qualified Scene.Load as Load

initialize :: SystemW ()
initialize = do
  void $ newEntity (Window 0 0)
  void $ newEntity Cursor
  void $ newEntity initialFoom

  Load.initialize
