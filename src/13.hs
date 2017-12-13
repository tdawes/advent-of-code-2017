import Data.String.Utils (split)

type Wall = (Int, Int)

parseInt :: [Char] -> Int
parseInt x = read x :: Int

parseInput :: [[Char]] -> Wall
parseInput (x:y:[]) = (parseInt $ head $ split ":" x, parseInt y)

position :: Int -> Int -> Int
position t r | p < r = p
             | otherwise = (2 * (r - 1)) - p
  where p = t `mod` (2 * (r - 1))

severity :: Wall -> Int
severity (d, r) | position d r == 0 = (d * r)
                | otherwise = 0

main = do
  input <- map (parseInput . words) <$> lines <$> getContents

  -- Part A
  print $ sum $ map severity input
