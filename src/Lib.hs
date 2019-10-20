{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Apecs
import Apecs.Gloss
import Control.Lens
import Linear.V2

-- * Components

-- ** Game domain

-- *** Player-owned things

-- | Think of the children!
data City = City
  deriving (Show)

instance Component City where
  type Storage City = Map City

-- | Shoots stuff out of the sky.
newtype Silo = Silo { siloStockpile :: Int }
  deriving (Show)

instance Component Silo where
  type Storage Silo = Map Silo

-- | There are many missiles, but those are yours.
newtype Intercept = Intercept Position
  deriving (Show)

instance Component Intercept where
  type Storage Intercept = Map Intercept

-- *** Enemy-owned things

-- | Oldie but goodie. One of those is enough to ruin a city.
newtype Missile = Missile Position
  deriving (Show)

instance Component Missile where
  type Storage Missile = Map Missile

-- | Masquerades for the ordinary then suddenly splits up.
newtype MIRV = MIRV [Position]
  deriving (Show)

instance Component MIRV where
  type Storage MIRV = Map MIRV

-- | Smart-ass projectiles that aren't too dumb to get into blasts.
newtype SmartBomb = SmartBomb Position
  deriving (Show)

instance Component SmartBomb where
  type Storage SmartBomb = Map SmartBomb

-- | Travels from edge to edge raining death and destruction.
data Bomber = Bomber
  deriving (Show)

instance Component Bomber where
  type Storage Bomber = Map Bomber

-- | Invaders from the outer space!
data Alien = Alien
  deriving (Show)

instance Component Alien where
  type Storage Alien = Map Alien

-- *** Shared

-- | It would be a blast!
newtype Blast = Blast { blastRadius :: Float }
  deriving (Show)

instance Component Blast where
  type Storage Blast = Map Blast

-- ** UI and controls

data Cursor = Cursor
  deriving (Show)

instance Component Cursor where
  type Storage Cursor = Unique Cursor

-- | Resize handler store.
data Window = Window
  { _screenWidth  :: Int
  , _screenHeight :: Int
  } deriving (Show)

instance Component Window where
  type Storage Window = Unique Window

-- ** Common aspects

newtype Position = Position (V2 Float)
  deriving (Eq, Ord, Show)

instance Component Position where
  type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float)
  deriving (Eq, Ord, Show)

instance Component Velocity where
  type Storage Velocity = Map Velocity

newtype Direction = Direction Float
  deriving (Eq, Ord, Show)

instance Component Direction where
  type Storage Direction = Map Direction

newtype Timed = Timed Float
  deriving (Eq, Ord, Show)

instance Component Timed where
  type Storage Timed = Map Timed

-- * The world

makeWorld "World"
  [ ''Camera
  , ''Cursor
  , ''Window

  , ''City
  , ''Silo
  , ''Intercept

  , ''Missile
  , ''MIRV
  , ''SmartBomb
  , ''Bomber
  , ''Alien

  , ''Position
  , ''Velocity
  -- , ''Acceleration

  , ''Direction
  -- , ''AngularVelocity
  -- , ''AngularAccel

  , ''Timed
  ]

type SystemW a = System World a

-- * Lenses

makeLenses ''Window
