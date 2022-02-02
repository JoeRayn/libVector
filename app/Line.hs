module Line where

import Control.Monad
import Data.List
import Data.Vector as V (Vector, findIndex, fromList, (!), (!?))
import GHC.Base (undefined)

-- ToDo replace zero checks with close to zero within some tolerance

data Line a = Line
  { normalVector :: Vector a, -- a normal vector is orthagonal vector to the line
    constantTerm :: Double
  }

-- | Construct a line with the standard formular, if a zero vector is supplied as the normal vector then the line would just be a point so return Nothing.
line normalVector constantTerm = if any (> 0) normalVector then Just $ Line normalVector constantTerm else Nothing

-- instance Show Line where

isParallel :: Line a -> Line a -> Bool
isParallel = undefined

isEqual :: Line a -> Line a -> Bool
isEqual = undefined

intersection :: Line a -> Line a -> Either String (Vector a)
intersection = undefined

-- this is mostly error handleing code for if the lines normal vector is zero
-- i.e. its not a line. Does not seem very elagent to me.

-- | Calculate the a base vector for a line
baseVector :: (Num a, Eq a) => Line a -> Either String (Vector a)
baseVector (Line v c) = case initialIndex of
  Nothing -> Left "Zero vector can not be a normal vector for a line"
  (Just index) -> let initialCoefficient = v V.! index in Right $ V.fromList (replicate index 0 ++ [initialCoefficient] ++ replicate (length v - (index + 1)) 0)
  where
    initialIndex = V.findIndex (/= 0) v

-- alternative implementation of baseVector in the maybe monad
baseVector' :: (Num a, Eq a) => Line a -> Maybe (Vector a)
baseVector' (Line v c) = do
  initialIndex <- V.findIndex (/= 0) v
  initialCoefficient <- v V.!? initialIndex
  let head = replicate initialIndex 0
  let tail = replicate (length v - initialIndex + 1) 0
  return (V.fromList (head ++ [initialCoefficient] ++ tail))

-- initialCoefficient >>= \x -> Just $ V.fromList (head +++ tail)
-- where initialIndex = V.findIndex (/= 0) v
--       initialCoefficient = initialIndex >>= (v V.!?)
--       head =  fmap (0 `replicate`) initialIndex
--       tail =  fmap (\x -> replicate (length v - (x + 1)) 0) initialIndex
--       (+++) = liftM (++)

-- (Just index) -> let initialCoefficient = v V.! index in Just $ V.fromList (replicate index 0 ++ [initialCoefficient] ++ replicate (length v - (index + 1)) 0)
-- where
--   initialIndex =
