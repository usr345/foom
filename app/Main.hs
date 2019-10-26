module Main where

import Apecs
import Apecs.Gloss

import World (initWorld)
import World.Init (initialize)
import World.Draw (draw)
import World.Input (onInput)
import World.Tick (onTick)

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play FullScreen black 60 draw onInput onTick
