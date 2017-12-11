import Data.String.Utils
import Data.List (inits)

sum3 :: (Num a) => [(a, a, a)] -> (a, a, a)
sum3 [] = (0, 0, 0)
sum3 ((x,y,z):cs) = (x + xs, y + ys, z +zs)
  where (xs, ys, zs) = sum3 cs

toHex :: [Char] -> (Int, Int, Int)
toHex "n" = (1, 1, 0)
toHex "s" = (-1, -1, 0)
toHex "ne" = (0, 1, 1)
toHex "nw" = (1, 0, -1)
toHex "sw" = (0, -1, -1)
toHex "se" = (-1, 0, 1)

dist :: (Int, Int, Int) -> Int
dist (x, y, z) =  ((abs x) + (abs y) + (abs z)) `quot` 2

distance :: [[Char]] -> Int
distance dirs = dist $ sum3 $ map toHex dirs

maxDistance :: [[Char]] -> Int
maxDistance dirs = maxDistance (0, 0, 0) dirs
  where maxDistance pos [] = dist pos
        maxDistance pos (d:ds) = max (dist newPos) (maxDistance newPos ds)
          where newPos = sum3 [pos, toHex d]
main = do
  input <- split "," <$> head <$> lines <$> getContents

  -- Part A
  print $ distance input

  -- Part B
  print $ maxDistance input
