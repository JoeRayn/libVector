module Line where

import Data.Vector

data Line a = Line
  { normalVector :: Vector a,
    constantTerm :: Double
  }

-- instance Show Line where

isParallel :: Line a -> Line a -> Bool
isParallel = undefined

isEqual :: Line a -> Line a -> Bool
isEqual = undefined

intersection :: Line a -> Line a -> Either String (Vector a)
intersection = undefined
