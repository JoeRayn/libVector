{-# LANGUAGE DeriveAnyClass #-}

module Line where

import Control.Monad
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V (cons, empty, findIndex, foldr, fromList, replicate, singleton, (!), (!?), (++))
import GHC.Base (undefined)
import Point
import Test.Tasty.QuickCheck as QC

-- ToDo replace zero checks with close to zero within some tolerance

data Line a = Line
  { normalVector :: Vector a, -- a normal vector is orthagonal vector to the line
    constantTerm :: a
  }

-- | Construct a line with the standard formular, if a zero vector is supplied as the normal vector then the line would just be a point so return Nothing.
line :: (Eq a, Num a) => Vector a -> a -> Maybe (Line a)
line normalVector constantTerm = if any (/= 0) normalVector then Just $ Line normalVector constantTerm else Nothing

maybeLineFromList x = line (V.fromList x)

lineFromList x = Line (V.fromList x)

instance Show a => Show (Line a) where
  show (Line v c) = show v ++ " " ++ show c

isParallel :: (Floating a, Ord a) => Line a -> Line a -> Bool
isParallel a b = isOrthagonal (normalVector a) (normalVector b)

isEqual :: Line a -> Line a -> Bool
isEqual = undefined

intersection :: Line a -> Line a -> Either String (Vector a)
intersection = undefined

-- this is mostly error handleing code for if the lines normal vector is zero
-- i.e. its not a line. Does not seem very elagent to me.

-- | Calculate the a base vector for a line
baseVector :: (Floating a, Eq a) => Line a -> Either String (Vector a)
baseVector (Line v c) = do
  initialIndex <- maybeToEither "Zero vector can not be a normal vector for a line" (V.findIndex (/= 0) v)
  initialCoefficient <- maybeToEither "Index not found" (v V.!? initialIndex)
  let head = V.replicate initialIndex 0
  let tail = V.replicate (length v - (initialIndex + 1)) 0
  return (head V.++ V.singleton (c / initialCoefficient) V.++ tail)

-- | Alternative implementation of basevector using fold
baseVector' :: (Eq a, Floating a) => Line a -> Either String (Vector a)
baseVector' l
  | any (/= 0) bv = Right bv
  | otherwise = Left "Zero vector can not be a normal vector for a line"
  where
    bv = baseVectorPossiblyZero l

-- | Calculate the base vector for a line, if the line. Does not check that the line is well formed. Not sure if the cons prepend operator (cons) is effecient on arrays.
baseVectorPossiblyZero :: (Eq a, Floating a) => Line a -> Vector a
baseVectorPossiblyZero (Line v c) = V.foldr dimention V.empty v
  where
    dimention a as =
      if a == 0
        then V.cons 0 as
        else V.cons (c / a) (V.replicate (length as) 0)

-- | Convert Maybe to Either
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = (`maybe` Right) . Left

instance (Arbitrary a, Num a) => Arbitrary (Line a) where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    x <- arbitrary
    z <- arbitrary
    case n of
      0 -> lineFromList [0, x] <$> arbitrary
      1 -> lineFromList [x, 0] <$> arbitrary
      2 -> lineFromList [x, z] <$> arbitrary

prop_baseVector :: Line Double -> Bool
prop_baseVector x = baseVector x == baseVector' x

prop_direction_vector_orthagornal_to_normal_vector :: Line Double -> Bool
prop_direction_vector_orthagornal_to_normal_vector x = True --isOrthagonal (baseVectorPossiblyZero x) (normalVector x)
-- (not $ isZero $ normalVector x) ==>

tests = [QC.testProperty "baseVector_model" prop_baseVector, QC.testProperty "normal vector is orthagonal to direction vector" prop_direction_vector_orthagornal_to_normal_vector]
