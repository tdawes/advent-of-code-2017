
findNearestSquareRoot :: Int -> Int
findNearestSquareRoot x = floor $ sqrt $ fromIntegral (x - 1)

parseInput :: [Char] -> Int
parseInput x = read x :: Int

shift :: (Int, Int) -> (Int, Int) -> (Int, Int)
shift (x, y) (c, d) = (x + c, y + d)

calculateCoords :: Int -> (Int, Int)
calculateCoords 0 = (0, 0)
calculateCoords x = shift (calculateCoords (x-1)) delta
  where delta = calculateDelta x

calculateDelta :: Int -> (Int, Int)
calculateDelta x | (r `mod` 2 == 0 && x <= r ^ 2 + r) = (1, 0)
                 | (r `mod` 2 == 0 && x > r ^ 2 + r) = (0, -1)
                 | (r `mod` 2 == 1 && x <= r ^ 2 + r) = (-1, 0)
                 | (r `mod` 2 == 1 && x > r ^ 2 + r) = (0, 1)
                 where r = findNearestSquareRoot x

main = do
  input <- parseInput <$> getContents
  let coords = calculateCoords (input - 1)
  print $ (abs $ fst coords) + (abs $ snd coords)
