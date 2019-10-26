{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module World where

import Control.Monad.Reader (asks)

import Apecs (Has(..), Storage, SystemT(..), explInit, makeWorld)
import Apecs.Gloss (Camera)

import World.Components -- TODO: qualify
import qualified Scene.Intro.Components as Intro

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

  , ''Intro.IntroState
  ]

type SystemW a = SystemT World IO a
