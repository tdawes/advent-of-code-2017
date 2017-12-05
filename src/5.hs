import qualified Data.Map as M

parseInput :: [Char] -> Int
parseInput x = read x :: Int

type State = (Int, M.Map Int Int)

step :: (Int -> Int) -> State -> State
step update (p, d) = (newPosition, newData)
  where v = d M.! p
        newPosition = p + v
        newData = M.insert p (update v) d

numSteps :: (Int -> Int) -> M.Map Int Int -> Int
numSteps update d = numSteps' (0, d) 0
  where numSteps' (p, d) steps | p < 0 = steps
                               | p >= M.size d = steps
                               | otherwise = numSteps' (step update (p, d)) (steps + 1)

main = do
  input <- map parseInput <$> lines <$> getContents
  let mappedInput = M.fromList $ zip [0..length input] input

  -- Part A
  print $ numSteps (\d -> d + 1) mappedInput

  -- Part B
  print $ numSteps (\d -> if d >= 3 then d - 1 else d + 1) mappedInput
