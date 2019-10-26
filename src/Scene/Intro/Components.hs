{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Scene.Intro.Components where

import Apecs (Component(..), Global, Map, Unique)
import Control.Lens (makeLenses, makePrisms)
import Linear.V2 (V2(..))

data IntroState = IntroState
  { _isOffset :: Float
  , _isScaleX :: Float
  , _isScaleY :: Float
  }
  deriving (Show)

instance Component IntroState where
  type Storage IntroState = Unique IntroState

makeLenses ''IntroState
