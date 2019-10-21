module Main where

import Control.Monad (when, void)
import Data.Foldable (for_)
import Data.List (sort)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Apecs
import Apecs.Gloss
import Control.Lens
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
      ( City
      , Position $ V2 x 0
      )

draw :: SystemW Picture
draw = do
  cursor <- foldDraw $ \(Cursor, Position (V2 curX curY)) ->
    translate curX curY .
      color red $
        circle 4

  let
    terrain =
      translate 0 (-50) .
        color (dark green) $
          rectangleSolid 800 100

  cities <- foldDraw $ \(City, Position (V2 px py)) ->
    translate px py .
      color (bright blue) $
        rectangleSolid 30 10

  silos <- foldDraw $ \(Silo ammo, Position (V2 px py)) ->
    translate px py $ mconcat
      [ color red $
          rectangleSolid 30 10
      , color cyan $
          translate (-40) 30 . scale 0.5 0.25 $
            text (show ammo)
      ]

  intercepts <- foldDraw $ \(i, Position pos) -> do
    let Position o = i ^. interceptOrigin
    mconcat
      [ drawTrail blue o pos
      , drawMissile cyan pos
      ]

  missiles <- foldDraw $ \(m, Position pos) -> do
    let o = m ^. missileOrigin . _Position
    mconcat
      [ drawTrail (dim red) o pos
      , drawMissile orange pos
      ]

  blasts <- foldDraw $ \(b, Position (V2 px py)) ->
    translate px py $
      drawBlast b

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
      , cursor
      ]

  pure $ scene <> ui

drawTrail :: Color -> V2 Float -> V2 Float -> Picture
drawTrail c (V2 ox oy) (V2 px py) =
  color c $
    line [ (ox, oy), (px, py) ]

drawMissile :: Color -> V2 Float -> Picture
drawMissile c (V2 px py) =
  translate px py . -- rotate phi .
    color c $
      circle 2

drawBlast :: Blast -> Picture
drawBlast b = color c $ circleSolid r
  where
    (c, r) = case b ^. blastPhase of
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
      let dst = windowToWorld cam (winX, winY) + V2 0 200

      armed <- flip cfoldM [] $ \acc (Silo ammo, Position pos, silo) ->
        if ammo > 0 then
          pure $ (distanceA dst pos, pos, silo) : acc
        else
          pure acc

      -- liftIO $ print armed

      -- TODO: pick closest to target
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

  interceptorBlast
  missileBlast

  launchMissiles

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

missileBlast :: SystemW ()
missileBlast =
  cmap $ \(m, Position pos) ->
    -- XXX: can overshoot on laggy frames
    if distanceA (m ^. missileTarget . _Position) pos <= 10 then
      Left
        ( Blast BlastGrowing 0
        , Velocity $ V2 0 25
        , Not @Missile
        )
    else
      Right ()

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
