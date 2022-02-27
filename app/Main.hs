module Main where

import Control.Monad
import Line

main :: IO ()
main =
  print $ liftM2 isParallel (lineFromList [1.0, 1.0] 1.0) $ lineFromList [1.0, 2.0] 1.0
