{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module World where

import Control.Monad.Reader (asks)

import Apecs (Has(..), Storage, SystemT(..), explInit, makeWorld)
import Apecs.Gloss (Camera)

import World.Components -- TODO: qualify?

import qualified Scene.Intro.Components as Intro
import qualified Utils.Debug as Debug

-- * The world

makeWorld "World"
  [ ''Camera
  , ''Cursor
  , ''Window
  , ''Shade
  , ''Time

  , ''Scene
  , ''Foom

  , ''Intro.HoloScreen
  , ''Intro.HoloRay

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

  , ''Debug.Measure
  ]

type SystemW a = SystemT World IO a
