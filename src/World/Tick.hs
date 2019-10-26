module World.Tick where

import Apecs (cmap, global)
import Linear.V2 (V2(..))

import qualified Apecs as Entity

import Component
import World (SystemW)

import qualified Scene.Load as Load
import qualified Scene.Intro as Intro
import qualified Scene.Gameplay as Gameplay
import qualified Scene.Outro as Outro

onTick :: Float -> SystemW ()
onTick dt = do
  stepTime dt
  stepPosition dt

  Entity.get global >>= \case
    Load ->
      Load.onTick dt
    Intro ->
      Intro.onTick dt
    Gameplay  ->
      Gameplay.onTick dt
    Outro ->
      Outro.onTick dt

stepTime :: Float -> SystemW ()
stepTime dt =
  Entity.modify global $ \(Time t) -> Time (t + dt)

stepPosition :: Float -> SystemW ()
stepPosition dt = cmap $ \(Position pos, Velocity vel) ->
  Position $ pos + vel * V2 dt dt

onTickOutro :: Float -> SystemW ()
onTickOutro _dt =
  pure ()
