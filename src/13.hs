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

caught :: Int -> Wall -> Bool
caught delay (d, r) = position (d + delay) r == 0

severity :: Int -> Wall -> Int
severity delay (d, r) | caught delay (d, r) = (d * r)
                      | otherwise = 0

main = do
  input <- map (parseInput . words) <$> lines <$> getContents

  -- Part A
  print $ sum $ map (severity 0) input

  -- Part B
  print $ head [ t | t <- [0..], all (\w -> not $ caught t w) input ]
