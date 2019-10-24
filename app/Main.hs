module Main where

import Apecs
import Apecs.Gloss

import World (initWorld)
import System.Init (initialize)
import System.Draw (draw)
import System.Input (onInput)
import System.Tick (onTick)

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play FullScreen black 60 draw onInput onTick
