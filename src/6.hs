import qualified Data.Set as Set

parseInput :: [Char] -> Int
parseInput x = read x :: Int

nextList :: [Int] -> [Int]
nextList list = [ y + s | (x, i) <- indexedList,
                      let y = if i == maxIndex then 0 else x,
                      let s = maxValue `quot` l + if (i - 1 - maxIndex + l) `mod` l < maxValue `rem` l then 1 else 0 ]
  where l = length list
        indexedList = zip list [0..]
        (maxValue, maxIndex) = foldl1 (\(x, i) (y, j) -> if y > x then (y, j) else (x, i)) indexedList

numSteps :: [Int] -> Int
numSteps list = numSteps' list Set.empty 0
  where numSteps' list existing steps | list `Set.member` existing = steps
                                      | otherwise = numSteps' (nextList list) (Set.insert list existing) (steps + 1)

main = do
  input <- map parseInput <$> words <$> getContents

  -- Part A
  print $ numSteps input
