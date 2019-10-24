module System.Draw where

import Apecs
import Apecs.Gloss
import Control.Lens hiding (set)
import Linear.V2 (V2(..))

import Component
import World (SystemW)

draw :: SystemW Picture
draw = do
  terrain <- drawTerrain

  cities <- foldDraw drawCity
  silos <- foldDraw drawSilo

  intercepts <- foldDraw drawIntercept
  missiles <- foldDraw drawMissile
  mirvs <- foldDraw drawMirv

  blasts <- foldDraw drawBlast

  cursor <- foldDraw drawCursor
  score <- foldDraw drawScore

  let
    scene = translate 0 (-200) $ mconcat
      [ intercepts
      , missiles
      , mirvs
      , terrain
      , cities
      , silos
      , blasts
      ]

    ui = mconcat
      [ color red $ rectangleWire 800 600
      , color blue $ rectangleWire 1600 900
      , score
      , cursor
      ]

  pure $ scene <> ui

drawTerrain :: SystemW Picture
drawTerrain = do
  score <- get global
  let
    hits = fromIntegral $ score ^. groundHits
    greyish = greyN . max 0.1 $ 1 - sqrt hits / 10
    brown = makeColorI 139 69 19 255

    groundColor = if
      | hits == 0 ->
          green
      | hits <= 15 ->
          mixColors (1.0 - hits / 15) (hits / 15) green brown
      | hits <= 30 ->
          mixColors (1.0 - (hits - 15) / 15) ((hits - 15) / 15) brown greyish
      | otherwise ->
          greyish

  pure $
    translate 0 (-50) .
      color groundColor $
        rectangleSolid 800 100

drawCity :: (City, Position) -> Picture
drawCity (City ruined, Position (V2 px py)) =
  translate px py .
    color col $
      rectangleSolid 30 10
  where
    col =
      if ruined then
        greyN 0.25
      else
        kindaBlue

    kindaBlue = makeColor 0.2 0.3 1.0 1.0

drawSilo :: (Silo, Position) -> Picture
drawSilo (Silo ammo, Position (V2 px py)) =
  translate px py $ mconcat
    [ color col $
        rectangleSolid 30 10
    , color cyan $
        translate (-40) 30 . scale 0.33 0.17 $
          text (show ammo)
    ]
  where
    col =
      if ammo > 0 then
        red
      else
        darkRed

    darkRed = makeColor 0.3 0 0 1.0

drawIntercept :: (Intercept, Position) -> Picture
drawIntercept (i, Position pos) =
  mconcat
    [ drawTrail blue o pos
    , drawProjectile cyan pos
    , drawTarget
    ]
  where
    drawTarget =
      translate tx ty .
        color (withAlpha 0.7 cyan) $ mconcat
          [ line [(2, 2), (4, 4)]
          , line [(-2, 2), (-4, 4)]
          , line [(2, -2), (4, -4)]
          , line [(-2, -2), (-4, -4)]
          ]

    Position o = i ^. interceptOrigin
    Position (V2 tx ty) = i ^. interceptTarget

drawMissile :: (Missile, Position) -> Picture
drawMissile (m, Position pos) =
  mconcat
    [ drawTrail (withAlpha 0.5 red) o pos
    , drawProjectile orange pos
    ]
  where
    o = m ^. missileOrigin . _Position

drawMirv :: (MIRV, Position) -> Picture
drawMirv (m, Position pos) =
  mconcat
    [ drawTrail (withAlpha 0.5 red) o pos
    , drawProjectile orange pos
    ]
  where
    o = m ^. mirvOrigin . _Position

drawTrail :: Color -> V2 Float -> V2 Float -> Picture
drawTrail c (V2 ox oy) (V2 px py) =
  color c $
    line [ (ox, oy), (px, py) ]

drawProjectile :: Color -> V2 Float -> Picture
drawProjectile c (V2 px py) =
  translate px py . -- rotate phi .
    color c $
      circle 2

drawBlast :: (Blast, Position) -> Picture
drawBlast (b, Position (V2 px py)) =
  translate px py .
    color col $
      circleSolid r
  where
    (col, r) = case b ^. blastPhase of
      BlastGrowing ->
        ( white
        , b ^. blastTimer * 100
        )
      BlastBurning ->
        ( yellow
        , 20 + (b ^. blastTimer) * 4
        )
      BlastSmoking ->
        ( greyN $ 1 - b ^. blastTimer
        , max 0 (1 - b ^. blastTimer) * 20
        )

drawCursor :: (Cursor, Position) -> Picture
drawCursor (_, Position cur@(V2 curX curY)) =
  if insideUI cur then
    translate curX curY .
      color red $
        circle 4
  else
    mempty

drawScore :: Score -> Picture
drawScore Score{..} = mconcat
  [ translate (-400) (-340) .
      scale 0.25 0.25 .
        color cyan .
          text $ "Score: " <> show _interceptorHits
  , translate 0 (-340) .
      scale 0.25 0.25 .
        color magenta .
          text $ "Casualties: " <> show _cityHits <> "m"
  ]
