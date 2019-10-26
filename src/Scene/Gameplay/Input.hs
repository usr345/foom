module Scene.Gameplay.Input where

import Control.Monad (when)

import Data.List (sort)

import Apecs (cfoldM, cmap, global, newEntity)
import Apecs.Gloss (Camera(..), windowToWorld)
import Graphics.Gloss.Interface.IO.Interact (Event(..), Key(..), KeyState(..), SpecialKey(..), MouseButton(..))
import Control.Lens hiding (set)
import Linear.V2 (V2(..))
import Linear.Affine (distanceA)
import Linear.Metric (normalize)

import qualified Apecs as Entity

import Component
import World (SystemW)

import qualified Scene.Outro

onInput :: Event -> SystemW ()
onInput e =
  -- liftIO $ print e
  case e of
    EventResize (newW, newH) -> do
      Entity.modify global $ \cam -> cam
        { camOffset = 0
        , camScale = 1.5 -- TODO: calculate correct scale to fit
        }

      cmap $ \w -> w
        & screenWidth .~ newW
        & screenHeight .~ newH

    -- Mouse

    EventMotion (winX, winY) -> do
      cam <- Entity.get global
      let pos = Position $ windowToWorld cam (winX, winY)

      cmap $ \Cursor ->
        pos

    EventKey (MouseButton LeftButton) Down _mods (winX, winY) -> do
      cam <- Entity.get global

      let cur = windowToWorld cam (winX, winY)
      let dst@(V2 _dx dy) = cur + V2 0 200
          isSafe = dy > 25 -- XXX: prevent detonating too low to ruin a city
      when (insideUI cur && isSafe) $ do

        armed <- flip cfoldM [] $ \acc (Silo ammo, Position pos, silo) ->
          if ammo > 0 then
            pure $ (distanceA dst pos, pos, silo) : acc
          else
            pure acc

        case sort armed of
          [] ->
            -- No ammunition left, enjoy the blinkenlighten.
            pure ()

          (_dist, src, silo) : _ -> do
            let vel = normalize (dst - src) * 250

            _ <- newEntity
              ( Intercept
                  { _interceptOrigin = Position src
                  , _interceptTarget = Position dst
                  }
              , Position src
              , Velocity vel
              )

            Entity.modify silo $ \(Silo ammo) ->
              Silo (ammo - 1)

    -- Keyboard

    EventKey (SpecialKey KeyEsc) Up _mods _pos ->
      Scene.Outro.initialize

    _ ->
      pure ()
