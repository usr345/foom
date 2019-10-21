{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Lib where

import Apecs
import Apecs.Gloss
import Control.Lens
import Linear.V2 (V2)

-- * Components

-- ** Game domain

-- *** Player-owned things

-- | Think of the children!
newtype City = City { _cityRuined :: Bool }
  deriving (Show)

instance Component City where
  type Storage City = Map City

-- | Shoots stuff out of the sky.
newtype Silo = Silo { _siloStockpile :: Int }
  deriving (Show)

instance Component Silo where
  type Storage Silo = Map Silo

-- | There are many missiles, but those are yours.
data Intercept = Intercept
  { _interceptOrigin :: Position
  , _interceptTarget :: Position
  }
  deriving (Show)

instance Component Intercept where
  type Storage Intercept = Map Intercept

-- *** Enemy-owned things

-- | Oldie but goodie. One of those is enough to ruin a city.
data Missile = Missile
  { _missileOrigin :: Position
  , _missileTarget :: Position
  }
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
data Blast = Blast
  { _blastPhase :: BlastPhase
  , _blastTimer :: Float
  }
  deriving (Show)

instance Component Blast where
  type Storage Blast = Map Blast

data BlastPhase
  = BlastGrowing
  | BlastBurning
  | BlastSmoking
  deriving (Eq, Ord, Show)

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

-- | Resize handler store.
data Score = Score
  { _interceptorHits :: Int
  , _siloHits        :: Int
  , _cityHits        :: Int
  , _groundHits      :: Int
  } deriving (Show)

emptyScore :: Score
emptyScore = Score
  { _interceptorHits = 0
  , _siloHits        = 0
  , _cityHits        = 0
  , _groundHits      = 0
  }

instance Component Score where
  type Storage Score = Unique Score

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

-- * The world

makeWorld "World"
  [ ''Camera
  , ''Cursor
  , ''Window
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

type SystemW a = System World a

-- * Lenses

makeLenses ''Window
makeLenses ''Score

makeLenses ''City
makeLenses ''Silo
makeLenses ''Intercept
makeLenses ''Missile
makeLenses ''Blast

makePrisms ''Position
