module Utils.Draw where

-- import Data.List (foldl')

import Graphics.Gloss

textLines :: [String] -> Picture
textLines =
  translate 0 (-75) . foldr step mempty
  where
    step str lower =
      mappend
        (text str)
        (translate 0.0 (-150.0) lower)
