import qualified Data.Map as M
type Register = [Char]
type Command = (Register, Int, (M.Map Register Int -> Bool))

parseInt :: [Char] -> Int
parseInt x = read x :: Int

getComparison :: [Char] -> Int -> Int -> Bool
getComparison operator = compare
  where compare a b | operator == "<" = a < b
                    | operator == "<=" = a <= b
                    | operator == ">" = a > b
                    | operator == ">=" = a >= b
                    | operator == "==" = a == b
                    | operator == "!=" = a /= b

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

step :: M.Map Register Int -> Command -> M.Map Register Int
step s (register, delta, command) | command s = (M.insert register ((getValue s register) + delta) s)
                                  | otherwise = s


main = do
  input <- map parseInput <$> map words <$> lines <$> getContents

  -- Part A
  let finalStore = foldl step M.empty input
  print $ maximum $ map (\(register, value) -> value) $ M.toList finalStore
