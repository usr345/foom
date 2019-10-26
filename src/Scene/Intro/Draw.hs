module Scene.Intro.Draw where

import Data.Maybe (fromJust)

import Apecs
import Apecs.Gloss
-- import Control.Lens hiding (set)
-- import Linear.V2 (V2(..))

import qualified Apecs as Entity

-- import Scene.Intro.Components
import World.Components
import Utils.Draw (textLines)
import Utils.Debug (Measure(..))
import World (SystemW)

draw :: SystemW Picture
draw = do
  Time _t <- Entity.get global
  Measure{..} <- fromJust <$> cfold (\_ is -> Just is) Nothing
  let
    topMsg =
      translate (-400) 372 . scale 0.16 0.12 .
        color cyan $
          textLines
            [ "FOOM v0.8"
            , "Some other line"
            , "third line"
            , "whoa, there's a loads of space"
            ]
  let
    viewScreen =
      color red $
        rectangleWire 800 600
  let
    bottomMsg =
      translate (-400) (-312) . scale 0.16 0.12 .
        color cyan $
          textLines
            [ "Press any button to assume direct control."
            , "Some other line"
            , "third line"
            , "whoa, there's a loads of space"
            ]

  pure $ mconcat
    [ topMsg
    , viewScreen
    , scale 0.2 0.2 .
        color green .
          text . unwords $ map show [_mOffsetX, _mOffsetY, _mScaleX, _mScaleY]
    , bottomMsg
    ]
