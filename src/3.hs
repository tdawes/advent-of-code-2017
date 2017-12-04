import Data.Function.Memoize

parseInput :: [Char] -> Int
parseInput x = read x :: Int

shift :: (Int, Int) -> (Int, Int) -> (Int, Int)
shift (x, y) (c, d) = (x + c, y + d)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours c = [ shift c (x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]

getPosition :: (Int, Int) -> Int
getPosition (0, 0) = 1
getPosition (x, y) | x + y > 0 && x == r = (l-1)^2 + (y + r)
                   | x + y > 0 && y == r = (l-1)^2 + l + (r - x)
                   | x + y <= 0 && x == -r = (l-1)^2 + 1 + (r - y)
                   | otherwise = (l-1)^2 + l + (x + r)
  where r = max (abs x) (abs y)
        l = if x + y > 0 then 2 * r else 2 * r + 1

smallerNeighbours :: (Int, Int) -> [(Int, Int)]
smallerNeighbours (x, y) = [ n | n <- neighbours (x, y), getPosition n <= position ]
  where position = getPosition (x, y)

calculateCoords :: Int -> (Int, Int)
calculateCoords 1 = (0, 0)
calculateCoords x = shift (calculateCoords (x-1)) delta
  where delta = calculateDelta (x - 1)

calculateDelta :: Int -> (Int, Int)
calculateDelta 1 = (1, 0)
calculateDelta x | (r `mod` 2 == 0 && x > r ^ 2 + r) = (1, 0)
                 | (r `mod` 2 == 0 && x <= r ^ 2 + r) = (0, -1)
                 | (r `mod` 2 == 1 && x > r ^ 2 + r) = (-1, 0)
                 | (r `mod` 2 == 1 && x <=  r ^ 2 + r) = (0, 1)
                 where r = findNearestSquareRoot (x - 1)

findNearestSquareRoot :: Int -> Int
findNearestSquareRoot x = floor $ sqrt $ fromIntegral x

calculateWeightForCoords :: (Int, Int) -> Int
calculateWeightForCoords' (0, 0) = 1
calculateWeightForCoords' (x, y) = foldl1 (+) $ map calculateWeightForCoords $ smallerNeighbours (x, y)
calculateWeightForCoords = memoize calculateWeightForCoords' 

calculateWeight :: Int -> Int
calculateWeight = calculateWeightForCoords . calculateCoords

run :: Int -> Int -> Int
run score x = if c > score then c else run score (x+1)
    where c = calculateWeight x

main = do
  input <- parseInput <$> getContents
  -- let input = 10
  let score = run input 1
  print score
