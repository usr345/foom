module Scene.Gameplay
  ( initialize
  , draw
  , onTick
  , onInput
  ) where

import Control.Monad (void)
import Data.Foldable (for_)
import Debug.Trace (traceM)

import Apecs (global, newEntity, ($=))
import Linear.V2 (V2(..))

import Component
import Scene.Gameplay.Draw (draw)
import Scene.Gameplay.Tick (onTick)
import Scene.Gameplay.Input (onInput)
import World (SystemW)

initialize :: SystemW ()
initialize = do
  traceM "Gameplay: initialize"
  global $= Gameplay

  void $ newEntity emptyScore

  for_ slots $ \n ->
    if n `elem` [head slots, 0, last slots] then
      newSilo $ slotPos n
    else
      newCity $ slotPos n

  where
    slots = [-4 .. 4]

    slotPos n = 700 / fromIntegral (length slots - 1) * n

    newSilo x = newEntity
      ( Silo 10
      , Position $ V2 x 0
      )

    newCity x = newEntity
      ( City False
      , Position $ V2 x 0
      )
