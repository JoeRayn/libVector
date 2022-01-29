module Point where

import Data.Complex (magnitude)
import Data.Vector as V (Vector, any, fromList, zipWith, (!))

v :: [a] -> Vector a
v = V.fromList

-- | Add two vectors.
(<+>) :: Num a => Vector a -> Vector a -> Vector a
(<+>) = V.zipWith (+)

-- | Subtract two vectors.
(<->) :: Num a => Vector a -> Vector a -> Vector a
(<->) = V.zipWith (-)

-- | Multipi vector by a constant.
(|*|) ::
  Num a =>
  -- |
  a ->
  -- |
  Vector a ->
  Vector a
(|*|) = fmap . (*)

-- | Calulate if the magnitude of a vector.
mag ::
  (Floating b) =>
  -- |
  Vector b ->
  b
mag x = sqrt $ sum $ fmap (^ 2) x

-- | Normalise vector
norm ::
  (Floating a) =>
  -- |
  Vector a ->
  Vector a
norm x = (1 / mag x) |*| x

-- | Calulate the dot product of two vectors.
(<.>) ::
  Floating a =>
  Vector a ->
  Vector a ->
  a
(<.>) x y = sum $ V.zipWith (*) x y

data AngleUnit = Rad | Degrees deriving (Eq, Show)

-- | Calulate the angle between two vectors
angle ::
  (Eq a, Floating a) =>
  -- |  Rad or Degrees
  AngleUnit ->
  Vector a ->
  Vector a ->
  a
angle b x y = if b == Rad then rad else rad * (180 / pi)
  where
    rad = acos (x <.> y / (mag x * mag y))

-- | Calulate if a vector is zero to some tolerance.
isZero :: (Ord a, Floating a) => Vector a -> Bool
isZero x = not $ V.any (< 1.0e-10) x

-- | Calulate if a two vectors are parallel to some tolerance.
isParallel :: (Eq a, Ord a, Floating a) => Vector a -> Vector a -> Bool
isParallel x y
  | isZero x || isZero y = True
  | otherwise = a == pi || a == 0
  where
    a = angle Rad x y

-- | Calulate if two vetors are orthagonal to some tolerance
isOrthagonal :: (Ord a, Floating a) => Vector a -> Vector a -> Bool
isOrthagonal x y = abs (x <.> y) < 1.0e-10

projection :: (Floating a) => Vector a -> Vector a -> Vector a
projection x y = (a <.> y) |*| a where a = norm x

othagonalComponent :: (Floating a) => Vector a -> Vector a -> Vector a
othagonalComponent x y = y <-> projection x y

{- unsafe -}
crossProduct :: (Floating a) => Vector a -> Vector a -> Vector a
crossProduct v w =
  V.fromList
    [ (y1 * z2) - (y2 * z1),
      - (x1 * z2 - x2 * z1),
      x1 * y2 - x2 * y1
    ]
  where
    x1 = v ! 0
    x2 = w ! 0
    y1 = v ! 1
    y2 = w ! 1
    z1 = v ! 2
    z2 = w ! 2

paralleagram :: Floating b => Vector b -> Vector b -> b
paralleagram x y = mag $ crossProduct x y

triangle :: Floating a => Vector a -> Vector a -> a
triangle x y = 0.5 * paralleagram x y
