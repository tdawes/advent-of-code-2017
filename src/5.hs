parseInput :: [Char] -> Int
parseInput x = read x :: Int

type State = (Int, [Int])

updateData :: [Int] -> Int -> [Int]
updateData (d:ds) 0 = (d+1):ds
updateData (d:ds) n = d:(updateData ds (n-1))

step :: State -> State
step (p, d) = (newPosition, newData)
  where newPosition = p + (d !! p)
        newData = updateData d p

numSteps :: State -> Int
numSteps (p, d) | p < 0 = 0
                | p >= (length d) = 0
                | otherwise = 1 + (numSteps $ step (p, d))

main = do
  input <- map parseInput <$> lines <$> getContents

  -- Part A
  print $ numSteps (0, input)
