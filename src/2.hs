comparedElement :: (Num a) => [a] -> Int -> Int
comparedElement list index = (index + (quot (length list) 2)) `mod` (length list)

parseInput :: [Char] -> Int
parseInput x = read x :: Int

calculateValue :: (Num a, Ord a) => [a] -> a
calculateValue line = abs $ (foldl1 max line) - (foldl1 min line)

main = do
  lines <- map (map parseInput) <$> map words <$> lines <$> getContents
  let diffs = map calculateValue lines
  let sum = foldl1 (+) diffs
  print sum
