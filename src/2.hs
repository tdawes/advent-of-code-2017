import Data.List

comparedElement :: (Num a) => [a] -> Int -> Int
comparedElement list index = (index + (quot (length list) 2)) `mod` (length list)

parseInput :: [Char] -> Int
parseInput x = read x :: Int

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

sortElems :: (Ord a) => (a, a) -> (a, a)
sortElems (x, y) = if x > y then (x, y) else (y, x)

doDivision :: Maybe Int -> (Int, Int) -> Maybe Int
doDivision a (x, y) = if (rem x y) == 0 then Just $ quot x y else a

calculateValue :: [Int] -> Maybe Int
calculateValue line = foldl doDivision Nothing $ map sortElems $ pairs line

main = do
  lines <- map (map parseInput) <$> map words <$> lines <$> getContents
  let diffs = map calculateValue lines
  print $ fmap sum $ sequence diffs
