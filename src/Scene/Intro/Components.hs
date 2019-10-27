{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Scene.Intro.Components where

import Apecs (Component(..), Map)
import Apecs.Gloss (Color)
import Control.Lens (makeLenses)
import Linear.V2 (V2(..))

data HoloScreen = HoloScreen
  { _hsColor :: Color
  , _hsOpen  :: Float
  , _hsPos   :: V2 Float
  , _hsSize  :: V2 Float
  } deriving (Show)

instance Component HoloScreen where
  type Storage HoloScreen = Map HoloScreen

data HoloRay = HoloRay
  { _hrOrigin  :: V2 Float
  , _hrTarget  :: V2 Float
  , _hrColor   :: Color
  , _hrOpacity :: Float
  } deriving (Show)

instance Component HoloRay where
  type Storage HoloRay = Map HoloRay

makeLenses ''HoloScreen
makeLenses ''HoloRay
