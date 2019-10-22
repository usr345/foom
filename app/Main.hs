module Main where

import Control.Monad (when, void)
import Data.Foldable (for_)
import Data.List (sort)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Apecs
import Apecs.Gloss
import Control.Lens hiding (set)
import Linear.V2 (V2(..))
import Linear.Affine (distanceA)
import Linear.Metric (normalize)

import Lib

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play FullScreen black 60 draw onInput onTick

initialize :: SystemW ()
initialize = do
  void $ newEntity (Window 0 0)
  void $ newEntity Cursor
  void $ newEntity emptyScore

  for_ slots $ \n ->
    if n `elem` [head slots, 0, last slots] then
      newSilo $ slotPos n
    else
      newCity $ slotPos n

  where
    slots = [-4 .. 4]

    slotPos n = 700 / fromIntegral (length slots - 1) * n

    newSilo x = newEntity
      ( Silo 10
      , Position $ V2 x 0
      )

    newCity x = newEntity
      ( City False
      , Position $ V2 x 0
      )

draw :: SystemW Picture
draw = do
  terrain <- drawTerrain

  cities <- foldDraw drawCity
  silos <- foldDraw drawSilo

  intercepts <- foldDraw drawIntercept
  missiles <- foldDraw drawMissile

  blasts <- foldDraw drawBlast

  cursor <- foldDraw drawCursor
  score <- foldDraw drawScore

  let
    scene = translate 0 (-200) $ mconcat
      [ intercepts
      , missiles
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

onInput :: Event -> SystemW ()
onInput e =
  -- liftIO $ print e
  case e of
    EventResize (newW, newH) -> do
      modify global $ \cam -> cam
        { camOffset = 0
        , camScale = 1.5 -- TODO: calculate correct scale to fit
        }

      cmap $ \w -> w
        & screenWidth .~ newW
        & screenHeight .~ newH

    -- Mouse

    EventMotion (winX, winY) -> do
      cam <- get global
      let pos = Position $ windowToWorld cam (winX, winY)

      cmap $ \Cursor ->
        pos

    EventKey (MouseButton LeftButton) Down _mods (winX, winY) -> do
      cam <- get global

      let cur = windowToWorld cam (winX, winY)
      let dst@(V2 _dx dy) = cur + V2 0 200
          isSafe = dy > 25 -- XXX: prevent detonating too low to ruin a city
      when (insideUI cur && isSafe) $ do

        armed <- flip cfoldM [] $ \acc (Silo ammo, Position pos, silo) ->
          if ammo > 0 then
            pure $ (distanceA dst pos, pos, silo) : acc
          else
            pure acc

        case sort armed of
          [] ->
            -- No ammunition left, enjoy the blinkenlighten.
            pure ()

          (_dist, src, silo) : _ -> do
            let vel = normalize (dst - src) * 250

            _ <- newEntity
              ( Intercept
                  { _interceptOrigin = Position src
                  , _interceptTarget = Position dst
                  }
              , Position src
              , Velocity vel
              )

            modify silo $ \(Silo ammo) ->
              Silo (ammo - 1)

    -- Keyboard

    EventKey (SpecialKey KeyEsc) Up _mods _pos ->
      liftIO exitSuccess

    _ ->
      pure ()

onTick :: Float -> SystemW ()
onTick dt = do
  stepPosition dt
  stepBlast dt

  interceptorHit
  interceptorBlast

  launchMissiles
  missileHit

stepPosition :: Float -> SystemW ()
stepPosition dt = cmap $ \(Position pos, Velocity vel) ->
  Position $ pos + vel * V2 dt dt

stepBlast :: Float -> SystemW ()
stepBlast dt = cmap $ \(blastTimer +~ dt -> b) ->
  case b ^. blastPhase of
    BlastGrowing ->
      Right $
        if b ^. blastTimer >= 0.2 then
          b & blastPhase .~ BlastBurning
            & blastTimer .~ 0
        else
          b
    BlastBurning ->
      Right $
        if b ^. blastTimer >= 0.2 then
          b & blastPhase .~ BlastSmoking
            & blastTimer .~ 0
        else
          b
    BlastSmoking ->
      if b ^. blastTimer >= 0.2 then
        Left $ Not @(Blast, Position, Velocity)
      else
        Right b

interceptorBlast :: SystemW ()
interceptorBlast =
  cmap $ \(i, Position pos, Velocity vel) ->
    -- XXX: can overshoot on laggy frames
    if distanceA (i ^. interceptTarget . _Position) pos <= 10 then
      Left
        ( Blast BlastGrowing 0
        , Velocity $ vel / 5
        , Not @Intercept
        )
    else
      Right ()

interceptorHit :: SystemW ()
interceptorHit =
  cmapM_ $ \(Missile{}, Position mp, missile) ->
    cmapM_ $ \(b, Position bp) ->
      if distanceA mp bp <= 20 + (b ^. blastTimer) * 4 then do
        destroy missile $ Proxy @(Missile, Position, Velocity)
        cmap $ interceptorHits +~ 1
        cmap $ siloStockpile +~ 1
      else
        pure ()

launchMissiles :: SystemW ()
launchMissiles = do
  dice <- liftIO $ randomRIO @Float (0.0, 1.0)
  when (dice <= 0.01) $ do
    ox <- liftIO $ randomRIO (-400, 400)
    let o = V2 ox 300 + V2 0 200

    tx <- liftIO $ randomRIO (-400, 400)
    let t = V2 tx 0

    void $ newEntity
      ( Missile
          { _missileOrigin = Position o
          , _missileTarget = Position t
          }
      , Position o
      , Velocity $ normalize (t - o) * 75
      )

missileHit :: SystemW ()
missileHit =
  cmapM_ $ \(Missile{}, Position mp@(V2 _mx my), m) ->
    when (my <= 0) $ do
      modify global $ groundHits +~ 1

      cmapM_ $ \(City ruined, Position cp, c) ->
        when (not ruined && distanceA mp cp <= 35) $ do
          modify global $ cityHits +~ 1
          modify c $ cityRuined .~ True

      cmapM_ $ \(Silo ammo, Position sp, s) ->
        when (distanceA mp sp <= 35) $ do
          modify global $ siloHits +~ 1
          modify s $ siloStockpile .~
            max 0 (truncate @Float $ fromIntegral ammo / 2)

      destroy m $ Proxy @Missile
      set m $ Blast BlastGrowing 0
      set m $ Velocity $ V2 0 25

insideUI :: V2 Float -> Bool
insideUI (V2 cx cy) = and
  [ cx >= -400
  , cx <= 400
  , cy >= -300
  , cy <= 300
  ]
