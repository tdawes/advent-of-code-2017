import qualified Data.Map as M
import Data.Maybe (fromJust)

type Coordinate = (Int, Int)
type Grid = M.Map Coordinate Char
data Direction = U | L | D | R
data State = State Int Coordinate Direction Grid

turnLeft :: Direction -> Direction
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turnRight :: Direction -> Direction
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

move :: Coordinate -> Direction -> Coordinate
move (x, y) U = (x, y + 1)
move (x, y) R = (x + 1, y)
move (x, y) D = (x, y - 1)
move (x, y) L = (x - 1, y)

reverseDir :: Direction -> Direction
reverseDir = turnLeft . turnLeft

parseInput :: [[Char]] -> Grid
parseInput grid = M.fromList $ concat $ map (\(y, row) -> map (\(x, cell) -> ((x, y), cell)) $ zip [(-w)..w] row ) $ zip (reverse [(-h)..h]) grid
  where h = (length grid - 1) `quot` 2
        w = (length (grid !! 0) - 1) `quot` 2

safeGet :: Coordinate -> Grid -> Char
safeGet pos state | pos `M.member` state = state M.! pos
                  | otherwise = '.'

step :: State -> State
step (State count pos dir grid) | safeGet pos grid == '.' = State (count + 1) (move pos $ turnLeft dir) (turnLeft dir) (M.insert pos '#' grid)
                                | otherwise = State count (move pos $ turnRight dir) (turnRight dir) (M.insert pos '.' grid)

step' :: State -> State
step' (State count pos dir grid) | current == '.' = State count (move pos $ turnLeft dir) (turnLeft dir) (M.insert pos 'W' grid)
                                 | current == 'W' = State (count + 1) (move pos dir) dir (M.insert pos '#' grid)
                                 | current == '#' = State count (move pos $ turnRight dir) (turnRight dir) (M.insert pos 'F' grid)
                                 | otherwise = State count (move pos $ reverseDir dir) (reverseDir dir) (M.insert pos '.' grid)
  where current = safeGet pos grid

numBursts :: Int -> Grid -> (State -> State) -> Int
numBursts count initialGrid runStep = finalCount
  where initialState = State 0 (0, 0) U initialGrid
        (State finalCount _ _ _) = foldl (\x _ -> runStep x) initialState [1..count]

main = do
  input <- parseInput <$> lines <$> getContents

  -- Part A
  print $ numBursts 10000 input step

  -- Part B
  print $ numBursts 10000000 input step'
