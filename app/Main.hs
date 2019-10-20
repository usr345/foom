module Main where

import Data.Foldable (for_)
import System.Exit (exitSuccess)

import Apecs
import Apecs.Gloss
import Control.Lens
import Linear.V2

import Lib

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play FullScreen black 60 draw onInput onTick

initialize :: SystemW ()
initialize = do
  _ <- newEntity (Window 0 0)
  _ <- newEntity Cursor

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
        color green $
          rectangleSolid 800 100

  cities <- foldDraw $ \(City, Position (V2 px py)) ->
    translate px py .
      color blue $
        rectangleSolid 30 10

  silos <- foldDraw $ \(Silo _ammo, Position (V2 px py)) ->
    translate px py .
      color red $
        rectangleSolid 30 10

  intercepts <- foldDraw $ \(Intercept target, Position pos) -> do
    let V2 px py = pos
    let phi = const (pi) $ (target, pos)

    translate px py . rotate phi .
      color cyan $
        rectangleSolid 5 1

  let
    scene = translate 0 (-200) $ mconcat
      [ terrain
      , cities
      , silos
      , intercepts
      ]

    ui = mconcat
      [ color red $ rectangleWire 800 600
      , color blue $ rectangleWire 1600 900
      , cursor
      ]

  pure $ scene <> ui

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
      armed <- flip cfoldM [] $ \acc (Silo ammo, Position pos, silo) ->
        if ammo >=0 then
          pure $ (pos, silo) : acc
        else
          pure acc

      -- liftIO $ print armed

      -- TODO: pick closest to target
      case armed of
        [] ->
          -- No ammunition left, enjoy the blinkenlighten.
          pure ()

        (origin, silo) : _ -> do
          cam <- get global
          let target = windowToWorld cam (winX, winY) + V2 0 200
          -- TODO: launch intercept to position
          _ <- newEntity
            ( Position origin
            , Velocity 100
            , Direction $ pi / 3
            , Intercept $ Position target
            )

          modify silo $ \(Silo ammo) ->
            Silo (ammo - 1)

    -- Keyboard

    EventKey (SpecialKey KeyEsc) Up _mods _pos ->
      liftIO exitSuccess

    _ ->
      pure ()

onTick :: Float -> SystemW ()
onTick dt =
  -- XXX: there could be some Timed handling, but you can't destroy entities
  -- and the timeout handling is component-specific.
  stepPosition dt

stepPosition :: Float -> SystemW ()
stepPosition dt = cmap $ \(Position pos, Velocity vel) ->
  Position $ pos + vel * V2 dt dt
