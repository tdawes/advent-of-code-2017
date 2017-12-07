import Data.List
import Data.String.Utils
import Data.Maybe
import qualified Data.Set as Set
import Data.Map (fromListWith, toList)

type Node = ([Char], Int, [[Char]])

type Data = ([Char], Int)
data Tree a = Tip a | Node [Tree a] a deriving (Show, Eq)

parseWeight :: [Char] -> Int
parseWeight xs = read $ (tail . init) xs :: Int

parseChildren :: [[Char]] -> [[Char]]
parseChildren list = split "," (join "" list)

parseInput :: [[Char]] -> Node
parseInput (name:weight:_:children) = (name, parseWeight weight, parseChildren children)
parseInput [name, weight] = (name, parseWeight weight, [])

findRootNode :: [Node] -> Node
findRootNode nodes = head [ node | node <- nodes,
                                  let (name, _, _) = node,
                                  (find (\(other, _, children) -> other /= name && elem name children) nodes) == Nothing ]

findNode :: [Node] -> [Char] -> Node
findNode nodes nodeName = fromJust $ find (\(name, _, _) -> name == nodeName) nodes

toNode :: [Node] -> Node -> Tree Data
toNode nodes = toNode'
  where findNode' = findNode nodes
        toNode' (name, weight, []) = Tip (name, weight)
        toNode' (name, weight, children) = Node (map (toNode' . findNode') children) (name, weight)

constructTree :: [Node] -> Tree Data
constructTree nodes = toNode nodes rootNode
  where rootNode = findRootNode nodes

calculateWeight :: Tree Data -> Int
calculateWeight (Tip (_, weight)) = weight
calculateWeight (Node children (_, weight)) = weight + (sum $ map calculateWeight children)

optHead :: [a] -> Maybe a
optHead [] = Nothing
optHead (x:xs) = Just x

findInTree :: (Tree Data -> Bool) -> Tree Data -> Maybe (Tree Data)
findInTree predicate (Tip d) | predicate (Tip d) = Just (Tip d)
                             | otherwise = Nothing
findInTree predicate (Node children d) | predicate (Node children d) = Just (Node children d)
                                       | otherwise = optHead [ fromJust x | x <- map (findInTree predicate) children, x /= Nothing ]

isBalanced :: Tree Data -> Bool
isBalanced (Tip d) = True
isBalanced (Node children d) = Set.size (Set.fromList (map calculateWeight children)) == 1

isFirstUnbalanced :: Tree Data -> Bool
isFirstUnbalanced (Tip d) = False
isFirstUnbalanced (Node children d) = (all isBalanced children) && not (isBalanced (Node children d))

getData :: Tree Data -> Data
getData (Tip a) = a
getData (Node children a) = a

getChildren :: Tree Data -> [Tree Data]
getChildren (Tip a) = []
getChildren (Node children _) = children

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

getWeights :: [Tree Data] -> (Int, Int, Data)
getWeights nodes = (mc, lc, getData $ fromJust $ find (\x -> calculateWeight x == lc) nodes)
  where frequencies = frequency $ map calculateWeight nodes
        compareFrequencies x y = compare (snd x) (snd y)
        mc = fst $ maximumBy compareFrequencies frequencies
        lc = fst $ minimumBy compareFrequencies frequencies

main = do
  input <- map parseInput <$> map words <$> lines <$> getContents

  -- Part A
  print $ (\(name, _, _) -> name) $ findRootNode input

  -- Part B
  let unbalancedNode = fromJust $ findInTree isFirstUnbalanced $ constructTree input
  let (correctWeight, incorrectWeight, badNode) = getWeights $ getChildren unbalancedNode

  print $ (snd badNode) + (correctWeight - incorrectWeight)
