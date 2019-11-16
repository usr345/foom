module Config (Config(..), configuration) where

data Config = Config{
  windowWidth :: Float
  , windowHeight :: Float
  , interceptDistance :: Float
  , hitDist :: Float
  , speedMIRV :: Float
  , speedMissile :: Float
  , chanceMIRV :: Int
  , _MIRVLimit :: Int
  , _MissileLimit :: Int
  , chanceMissile :: Int
  , horizontalSpan :: Float
  , verticalSpan :: Float
  , groundLevelShift :: Float
  , _MIRVMinSpread :: Float
  , _MIRVMaxSpread :: Float
  , _MIRVMinHeads :: Int
  , _MIRVMaxHeads :: Int
                    }

configuration :: Config
configuration = Config
  {
    windowWidth = 1024
  , windowHeight = 768
  , interceptDistance = 10
  -- , velocityDiv = 5
  -- , hitDistance = 20
  -- , rio = 400
  , hitDist = 35
  , speedMIRV = 66
  , speedMissile = 75
  , _MIRVLimit = 1
  , _MissileLimit = 10
  -- 800 / 2
  , horizontalSpan = 400
  -- координата верхней точки в экранных координатах: 600 / 2
  , verticalSpan = 300
  , groundLevelShift = 200
  , chanceMIRV = 60 * 5
  , chanceMissile = 60 * 5
  , _MIRVMinSpread = 10
  , _MIRVMaxSpread = 15
  , _MIRVMinHeads = 2
  , _MIRVMaxHeads = 5
  }
