import Data.List (intercalate, find)
import Data.String.Utils (split)
import qualified Data.Set as Set
import Data.Maybe (fromJust)

type Node = (Int, [Int])

parseInt :: [Char] -> Int
parseInt x = read x :: Int

parseInput :: [[Char]] -> Node
parseInput (x:"<->":edges) = (parseInt x, map parseInt $ split "," $ intercalate "" edges )

findGroup :: Int -> [Node] -> Set.Set Int
findGroup x nodes = findGroup' nodes Set.empty x
  where findGroup' nodes visited x | Set.member x visited = Set.empty
                                   | otherwise= Set.insert x $ Set.unions $ map (findGroup' nodes newVisited) (neighbours x)
          where neighbours x = snd $ fromJust $ find (\node -> fst node == x) nodes
                newVisited = Set.insert x visited

groups :: [Node] -> [Set.Set Int]
groups [] = []
groups ns = newGroup:(groups [n | n <- ns, not $ Set.member (fst n) newGroup])
  where newGroup = findGroup (fst $ head ns) ns

main = do
  input <- map (parseInput . words) <$> lines <$> getContents

  -- Part A
  print $ Set.size $ findGroup 0 input

  -- Part B
  print $ length $ groups input
