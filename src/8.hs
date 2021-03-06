import qualified Data.Map as M
type Register = [Char]
type Command = (Register, Int, (M.Map Register Int -> Bool))

parseInt :: [Char] -> Int
parseInt x = read x :: Int

getComparison :: [Char] -> Int -> Int -> Bool
getComparison "<" = (<)
getComparison "<=" = (<=)
getComparison ">" = (>)
getComparison ">=" = (>=)
getComparison "==" = (==)
getComparison "!=" = (/=)

getValue :: M.Map Register Int -> Register -> Int
getValue m r | M.member r m = m M.! r
             | otherwise = 0

parseInput :: [[Char]] -> Command
parseInput (register:command:d:"if":compRegister:operator:value:[]) = (register, delta, \store -> (getValue store compRegister) `comp` (parseInt value))
  where delta = computeDelta command $ parseInt d
        comp = getComparison operator

computeDelta :: [Char] -> Int -> Int
computeDelta "inc" d = d
computeDelta "dec" d = (-1) * d

step :: [M.Map Register Int] -> Command -> [M.Map Register Int]
step (store:history) (register, delta, command) | command store = ((M.insert register ((getValue store register) + delta) store):store:history)
                                                | otherwise = (store:store:history)

safeMax :: (Ord a, Num a) => [a] -> a
safeMax [] = 0
safeMax xs = maximum xs

main = do
  input <- map parseInput <$> map words <$> lines <$> getContents
  let history = foldl step [M.empty] input

  -- Part A
  print $ safeMax $ map (\(register, value) -> value) $ M.toList $ head history

  -- Part A
  print $ safeMax $ map (safeMax . (map (\(register, value) -> value)) . M.toList) history
