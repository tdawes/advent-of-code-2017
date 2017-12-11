import Data.String.Utils
import Data.List (inits)

horizontalDistance :: [[Char]] -> Int
horizontalDistance dirs = abs $ sum $ map horiz dirs
  where horiz (_:[]) = 0
        horiz (_:'w':[]) = -1
        horiz (_:'e':[]) = 1

verticalDistance :: [[Char]] -> Int
verticalDistance dirs = abs $ sum $ map vert dirs
  where vert ('n':[]) = 2
        vert ('s':[]) = -2
        vert ('n':_) = 1
        vert ('s':_) = -1

distance :: [[Char]] -> Int
distance dirs | v > h = (floor $ (0.5 * (fromIntegral (v - h)))) + h
              | otherwise = h
  where v = verticalDistance dirs
        h = horizontalDistance dirs

main = do
  input <- split "," <$> head <$> lines <$> getContents

  -- Part A
  print $ distance input

  -- Part B
  print $ maximum $ map distance $ inits input
