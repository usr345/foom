{-# LANGUAGE TemplateHaskell #-}

module Utils.Debug where

import Control.Lens (makeLenses, (+~), (-~))

import Apecs (Component(..), EntityCounter, Has, Storage, SystemT, Unique, cmap, newEntity)
import Apecs.Gloss (Key(..))

data Measure = Measure
  { _mOffsetX :: Float
  , _mOffsetY :: Float
  , _mScaleX  :: Float
  , _mScaleY  :: Float
  } deriving (Show)

instance Component Measure where
  type Storage Measure = Unique Measure

makeLenses ''Measure

newMeasure_
  :: (Has w IO Measure, Has w IO EntityCounter)
  => SystemT w IO ()
newMeasure_ = () <$ newEntity (Measure 0 0 0 0)

-- | Key handler for 'Measure'.
measureOnKey :: Has w IO Measure => Key -> SystemT w IO ()
measureOnKey = \case
  Char 'a' ->
    cmap $ mOffsetX +~ -2
  Char 'd' ->
    cmap $ mOffsetX +~ 2
  Char 'w' ->
    cmap $ mOffsetY +~ -2
  Char 's' ->
    cmap $ mOffsetY +~ 2

  Char 'q' ->
    cmap $ mScaleX +~ 0.01
  Char 'e' ->
    cmap $ mScaleX -~ 0.01

  Char 'z' ->
    cmap $ mScaleY -~ 0.01
  Char 'x' ->
    cmap $ mScaleY +~ 0.01
  _ ->
    pure ()
