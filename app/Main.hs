module Main where

import Data.Vector as V (Vector, zipWith, fromList, any, (!))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn $ show $ (V.fromList [8.218, -9.341]) <+> (V.fromList [-1.129, 2.111])
  putStrLn $ show $ (V.fromList [7.119, 8.215]) <-> (V.fromList [-8.223, 0.878])
  putStrLn $ show $ 7.41 |*| (V.fromList [1.671, -1.012, -0.318])
  let v1 = [-0.221, 7.437] :: [Double]
  printV $ mag' $ V.fromList v1
  let v2 = [8.813, -1.331, -6.247]
  printV $ mag' $ V.fromList v2
  printV $ norm' t1
  printV $ norm' $ V.fromList [1.996, 3.108, -4.554]
  printV $ v [7.887, 4.138] <.> v [-8.802, 6.776]
  printV $ v [-5.955, -4.904, -1.874] <.> v [-4.496, -8.755, 7.103]
  printV $ angle Rad (v [3.183, -7.627]) (v [-2.668, 5.319])
  printV $ angle Degrees (v [7.35, 0.221, 5.188]) (v [2.751, 8.259, 3.985])
  let v3 = v [-7.579, -7.88]
  let v4 = v [22.737, 23.64]
  print $ isParallel v3 v4
  print $ isOrthagonal v3 v4
  print $ v3 <.> v4
  print $ angle Rad v3 v4
  let v5 = v [-2.029, 9.97, 4.172]
  let v6 = v [-9.231, -6.639, -2.94]
  let v7 = v [-2.328, -7.284, -1.214]
  let v8 = v [-1.821, 1.072, -2.94]
  let v9 = v [2.118, 4.827]
  let v10 = v [0.0, 0.0]
  putStrLn "q2"
  print $ isParallel v5 v6
  print $ isOrthagonal v5 v6
  print $ angle Rad v5 v6
  putStrLn "q3"
  print $ isParallel v7 v8
  print $ isOrthagonal v7 v8
  putStrLn "q4"
  print $ isParallel v9 v10
  print $ isOrthagonal v9 v10
  let v1 = v [3.039, 1.879]
  let b1 = v [0.825, 2.036]
  let v2 = v [-9.88, -3.264, -8.159]
  let b2 = v [-2.155, -9.353, -9.473]
  let v3 = v [3.009, -6.172, 3.692, -2.51]
  let b3 = v [6.404, -9.144, 2.759, 8.718]
  print $ projection b1 v1
  print $ othagonalComponent b2 v2
  print $ projection b3 v3
  print $ othagonalComponent b3 v3
  let v1 = v [8.462, 7.893, -8.187]
  let w1 = v [6.984, -5.975, 4.778]
  let v2 = v [-8.987, -9.838, 5.031]
  let w2 = v [-4.268, -1.861, -8.866]
  let v3 = v [1.5, 9.547, 3.691]
  let w3 = v [-6.007, 0.124, 5.772]
  print $ crossProduct v1 w1
  print $ paralleagram v2 w2
  print $ triangle v3 w3
  
  -- printV $ angle 

v :: [a] -> Vector a
v = V.fromList

v3 :: [Double]
t1 :: Vector Double
v3 = [5.581, -2.136] :: [Double]
t1 = V.fromList v3

printV :: Show a => a -> IO ()
printV = print

(<+>) :: Num a => Vector a -> Vector a -> Vector a
(<+>) = V.zipWith (+)

(<->) :: Num a => Vector a -> Vector a -> Vector a
(<->) = V.zipWith (-)

(|*|) :: Num a => a -> Vector a -> Vector a
(|*|) = fmap . (*)

mag :: (Integral a, Floating b) => Vector a -> b
mag x = sqrt $ fromIntegral $ sum $ fmap (^ 2) x

mag' :: (Floating b) => Vector b -> b
mag' x = sqrt $ sum $ fmap (^ 2) x

norm' :: (Floating a) => Vector a -- ^ 
  -> Vector a
norm' x = (1 / mag' x) |*| x

(<.>) :: Floating a => Vector a -> Vector a -> a
(<.>) x y = sum $ V.zipWith (*) x y

data AngleUnit = Rad | Degrees deriving (Eq, Show)

angle :: (Eq a, Floating a) => AngleUnit -> Vector a -> Vector a -> a
angle b x y = if b == Rad then rad else rad * (180/pi) where
  rad = acos ( x <.> y / (mag' x * mag' y))

isZero :: (Ord a, Floating a) => Vector a -> Bool
isZero x = not $ V.any (< 1.0e-10) x

isParallel :: (Eq a, Ord a, Floating a) => Vector a -> Vector a -> Bool
isParallel x y
  | isZero x || isZero y = True
  | otherwise = a == pi || a == 0
 where a = angle Rad x y

isOrthagonal :: (Ord a, Floating a) => Vector a -> Vector a -> Bool
isOrthagonal x y = abs (x <.> y) < 1.0e-10


projection :: (Floating a) => Vector a -> Vector a -> Vector a
projection x y = (a <.> y) |*| a where a = norm' x

othagonalComponent :: (Floating a) => Vector a -> Vector a -> Vector a
othagonalComponent x y = y <-> projection x y

crossProduct :: (Floating a) => Vector a -> Vector a -> Vector a
crossProduct v w = V.fromList [(y1 * z2) - (y2*z1),
                               -(x1 * z2 - x2 * z1),
                               x1*y2 - x2 * y1]
                   where
                     x1 = v ! 0  
                     x2 = w ! 0
                     y1 = v ! 1
                     y2 = w ! 1
                     z1 = v ! 2
                     z2 = w ! 2
                     

paralleagram :: Floating b => Vector b -> Vector b -> b
paralleagram x y = mag' $ crossProduct x y

triangle :: Floating a => Vector a -> Vector a -> a
triangle x y = 0.5 * paralleagram x y  
