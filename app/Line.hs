module Line where

import Data.Vector

data Line = Line baseVector constantTerm

-- instance Show Line where

isParallel :: Line -> Line -> Bool
isParallel = undefined

isEqual Line -> Line -> Bool
isEqual = undefined

intersection :: Line -> Line -> Either String Vector
intersection = undefined
