import Data.List

parseInput :: [Char] -> Int
parseInput x = read x :: Int

pairs :: (Eq a) => [a] -> [(a, a)]
pairs list = [ (x, y) | x <- list, y <- list, x /= y ]

main = do
  lines <- map (map parseInput) <$> map words <$> lines <$> getContents

  -- Part A
  print $ sum [ (maximum line) - (minimum line) | line <- lines ]

  -- Part B
  print $ sum [ quot x y | line <- lines, (x, y) <- pairs line, rem x y == 0 ]
