module Main where

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

  _ <- newEntity
    ( Silo 10
    , Position $ V2 0 0
    )

  _ <- newEntity
    ( Silo 10
    , Position $ V2 0 0
    )

  _ <- newEntity
    ( Silo 10
    , Position $ V2 0 0
    )

  _ <- newEntity
    ( City
    , Position $ V2 (-10) 0
    )
  _ <- newEntity
    ( City
    , Position $ V2 10 0
    )

  pure ()

draw :: SystemW Picture
draw = do
  -- Window winWidth winHeight <- get global

  cursor <- foldDraw $ \(Cursor, Position (V2 curX curY)) ->
    translate curX curY . color red $
      circle 4

  let
    terrain =
      translate 0 (-75) .
        color green $
          rectangleSolid 100 25

  cities <- foldDraw $ \(City, Position (V2 px py)) ->
    translate px py .
      color blue $
        rectangleSolid 10 10

  silos <- foldDraw $ \(Silo _ammo, Position (V2 px py)) ->
    translate px py .
      color red $
        rectangleSolid 10 10

  cam <- get global
  pure . cameraTransform cam $ mconcat
    [ terrain
    , cities
    , silos
    , cursor
    ]

onInput :: Event -> SystemW ()
onInput e =
  -- liftIO $ print e
  case e of
    EventResize (newW, newH) ->
      cmap $ \w -> w
        & screenWidth .~ newW
        & screenHeight .~ newH

    -- Mouse

    EventMotion (winX, winY) -> do
      cam <- get global
      let worldXY = windowToWorld cam (winX, winY)

      cmap $ \Cursor ->
        Position worldXY

    EventKey (MouseButton LeftButton) Down _mods (winX, winY) -> do
      cam <- get global
      let pos = Position $ windowToWorld cam (winX, winY)

      -- TODO: check closest silo with amunition remaining
      -- TODO: launch intercept to position
      pure ()

    -- Keyboard

    EventKey (SpecialKey KeyEsc) Up _mods _pos ->
      liftIO exitSuccess

    _ ->
      pure ()

onTick :: Float -> SystemW ()
onTick _dt =
  -- XXX: there could be some Timed handling, but you can't destroy entities
  -- and the timeout handling is component-specific.
  pure ()
