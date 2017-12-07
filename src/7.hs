import Data.List
import Data.String.Utils

parseWeight :: [Char] -> Int
parseWeight xs = read $ (tail . init) xs :: Int

parseChildren :: [[Char]] -> [[Char]]
parseChildren list = split "," (join "" list)

parseInput :: [[Char]] -> ([Char], Int, [[Char]])
parseInput (name:weight:_:children) = (name, parseWeight weight, parseChildren children)
parseInput [name, weight] = (name, parseWeight weight, [])

main = do
  input <- map parseInput <$> map words <$> lines <$> getContents

  -- Part A
  print $ head [ name | (name, _, _) <- input, (find (\(other, _, children) -> other /= name && elem name children) input) == Nothing ]
