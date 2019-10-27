module Scene.Intro.Tick where

import Control.Lens
import Control.Monad (void, when)
import System.Random (randomRIO)

import Apecs (cmap, cmapM, cmapM_, liftIO, newEntity)
import Apecs.Gloss (green)
import Linear.V2 (V2(..))

import Scene.Intro.Components (HoloRay(..), HoloScreen(..), hsOpen)
import World.Components -- (Scene(..), Shade(..), Time(..), initialFoom, foomProgress)
import World (SystemW)

import qualified Scene.Gameplay

onTick :: Float -> SystemW ()
onTick dt = do
  cmap $ \(Shade opaq) ->
    if opaq - dt <= 0 then
      Nothing
    else
      Just . Shade $ max 0 (opaq - dt)

  cmap $ \HoloRay{..} ->
    let
      opacity' = _hrOpacity - dt * 10
    in
      if opacity' <= 0 then
        Nothing
      else
        Just HoloRay{_hrOpacity=opacity', ..}

  cmapM $ \f@Foom{..} -> do
    when (_foomStatus == Activating) $ do
      cmap $ \hs -> hs
        & hsOpen .~ _foomProgress / 100

      when (_foomProgress < 95) $
        cmapM_ $ \HoloScreen{..} -> do
          let
            V2 sx sy = _hsSize
            oy = sy / 2 - sy * _hsOpen

          void $ newEntity HoloRay
            { _hrOrigin  = V2 (sx / 2) oy
            , _hrTarget  = V2 (negate $ sx / 2) oy
            , _hrColor   = green
            , _hrOpacity = 1.0
            }

    let
      progSpeed =
        case _foomStatus of
          Booting ->
            10
          _ ->
            90

      prog = min 100 $ (f ^. foomProgress) + dt * progSpeed

    if prog < 100 then
      pure $ f
        & foomProgress .~ prog
    else
      case f ^. foomStatus of
        Offline ->
          pure $ f
            & foomStatus .~ Booting
            & foomProgress .~ 0
        Booting ->
          pure $ f
            & foomStatus .~ Activating
            & foomProgress .~ 0
        _activated ->
          f <$ Scene.Gameplay.initialize
