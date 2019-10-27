module Scene.Intro
  ( initialize
  , draw
  , onInput
  , onTick
  ) where

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Debug.Trace (traceM)

import Apecs (cmap, global, newEntity, ($=))
import Apecs.Gloss (Event(..), KeyState(..), withGreen, black)
import Linear.V2 (V2(..))

-- import qualified Apecs as Entity

import Scene.Intro.Components (HoloScreen(..))
import World.Components
import World (SystemW)

import Scene.Intro.Draw (draw)
import Scene.Intro.Tick (onTick)

-- import qualified Utils.Debug as Debug

initialize :: SystemW ()
initialize = do
  traceM "Intro: initialize"
  -- Debug.newMeasure_
  void $ newEntity initialFoom
  void $ newEntity HoloScreen
    { _hsColor = withGreen 0.75 black
    , _hsOpen  = 0
    , _hsPos   = 0
    , _hsSize  = V2 800 600
    }

  global $= (Intro, Time 0)

onInput :: Event -> SystemW ()
onInput = \case
  EventKey _key Down _mods _pos ->
    cmap $ \f -> f
      & foomStatus .~ Activating
      & foomProgress .~ 0

    --   SpecialKey KeySpace ->
    --     Scene.Gameplay.initialize
    --   _ ->
    --     Debug.measureOnKey key
  _ ->
    pure ()
