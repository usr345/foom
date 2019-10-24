{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module World where

import Control.Monad.Reader (asks)

import Apecs (Has(..), Storage, SystemT(..), explInit, makeWorld)
import Apecs.Gloss (Camera)

import Component

-- * The world

makeWorld "World"
  [ ''Camera
  , ''Cursor
  , ''Window
  , ''Time

  , ''Scene
  , ''Score

  , ''City
  , ''Silo
  , ''Intercept

  , ''Missile
  , ''MIRV
  , ''SmartBomb
  , ''Bomber
  , ''Alien

  , ''Blast

  , ''Position
  , ''Velocity
  -- , ''Acceleration

  , ''Direction
  -- , ''AngularVelocity
  -- , ''AngularAccel

  ]

type SystemW a = SystemT World IO a
