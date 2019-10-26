module Scene.Intro
  ( initialize
  , draw
  , onInput
  , onTick
  ) where

import Control.Lens
import Control.Monad (void)
import Debug.Trace (traceM)

import Apecs (cmap, cmapM, global, newEntity, ($=))
import Apecs.Gloss (Event(..), Key(..), KeyState(..), SpecialKey(..))

-- import qualified Apecs as Entity

import World.Components -- (Scene(..), Shade(..), Time(..), initialFoom, foomProgress)
import World (SystemW)

import Scene.Intro.Draw (draw)

import qualified Scene.Gameplay
import qualified Utils.Debug as Debug

initialize :: SystemW ()
initialize = do
  traceM "Intro: initialize"
  Debug.newMeasure_
  void $ newEntity initialFoom
  global $= (Intro, Time 0)

onTick :: Float -> SystemW ()
onTick dt = do
  cmap $ \(Shade opaq) ->
    if opaq - dt <= 0 then
      Nothing
    else
      Just . Shade $ max 0 (opaq - dt)

  cmapM $ \f@Foom{..} -> do
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

onInput :: Event -> SystemW ()
onInput = \case
  EventKey _key Down _mods _pos ->
    cmap $ \f -> f
      & foomStatus .~ Activating
      & foomProgress .~ 0

    -- case key of
    --   SpecialKey KeyEsc ->
    --     -- XXX: prevent flash of gameplay before quit
    --     pure ()
    --   SpecialKey KeySpace ->
    --     Scene.Gameplay.initialize
    --   _ ->
    --     Debug.measureOnKey key
  _ ->
    pure ()
