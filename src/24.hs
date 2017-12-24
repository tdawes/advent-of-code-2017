import Data.String.Utils (split)
import Data.List (maximumBy, delete)

type Part = (Int, Int)
type Bridge = [Int]

parseInt :: [Char] -> Int
parseInt x = read x :: Int

parseInput :: [[Char]] -> Part
parseInput [x, y] = (parseInt x, parseInt y)

allBridges :: [Part] -> [Bridge]
allBridges parts = construct parts [0]

construct :: [Part] -> Bridge -> [Bridge]
construct [] existing = [existing]
construct ps existing = existing:(concat [ construct (delete (p, q) ps) (y:x:existing) | (p, q) <- ps,
                                                                                         p == h || q == h,
                                                                                         let (x, y) = if p == h then (p, q) else (q, p) ])
  where h = head existing

main = do
  input <- map parseInput <$> map (split "/") <$> lines <$> getContents

  -- Part A
  print $ maximum $ map sum $ allBridges input
