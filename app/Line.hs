module Line where

import Control.Monad
import Data.List
import Data.Vector as V (Vector, findIndex, fromList, (!), (!?))
import GHC.Base (undefined)

-- ToDo replace zero checks with close to zero within some tolerance

data Line a = Line
  { normalVector :: Vector a, -- a normal vector is orthagonal vector to the line
    constantTerm :: a}

-- | Construct a line with the standard formular, if a zero vector is supplied as the normal vector then the line would just be a point so return Nothing.
line normalVector constantTerm = if any (> 0) normalVector then Just $ Line normalVector constantTerm else Nothing

instance Show a => Show (Line a) where
  show (Line v c) = show v ++ " " ++ show c

isParallel :: Line a -> Line a -> Bool
isParallel = undefined

isEqual :: Line a -> Line a -> Bool
isEqual = undefined

intersection :: Line a -> Line a -> Either String (Vector a)
intersection = undefined

-- this is mostly error handleing code for if the lines normal vector is zero
-- i.e. its not a line. Does not seem very elagent to me.

-- | Calculate the a base vector for a line
baseVector' :: (Floating a, Eq a) => Line a -> Either String (Vector a)
baseVector' (Line v c) = do
  initialIndex <- maybeToEither "Zero vector can not be a normal vector for a line" (V.findIndex (/= 0) v)
  initialCoefficient <- maybeToEither "Index not found" (v V.!? initialIndex)
  let head = replicate initialIndex 0
  let tail = replicate (length v - initialIndex + 1) 0
  return (V.fromList (head ++ [c / initialCoefficient] ++ tail))

-- | Convert Maybe to Either
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = (`maybe` Right) . Left
