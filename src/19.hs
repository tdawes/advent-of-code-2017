import Data.List (findIndex)
import Data.Maybe (fromJust)

data Direction = L | R | D | U deriving Show
type Coordinate = (Int, Int)
type Maze = [[Char]]

findEntry :: Maze -> Coordinate
findEntry maze = (fromJust $ findIndex (\x -> x == '|') (maze !! 0), 0)

at :: Maze -> Coordinate -> Char
at maze (x, y) | y < 0 || y >= (length maze) || x < 0 || x >= (length (maze !! y)) = ' '
               | otherwise = maze !! y !! x

move :: Coordinate -> Direction -> Coordinate
move (x, y) D = (x + 1, y)
move (x, y) U = (x - 1, y)
move (x, y) L = (x, y - 1)
move (x, y) R = (x, y + 1)

turnAnticlockwise :: Direction -> Direction
turnAnticlockwise D = R
turnAnticlockwise R = U
turnAnticlockwise U = L
turnAnticlockwise L = D

turnClockwise :: Direction -> Direction
turnClockwise D = L
turnClockwise L = U
turnClockwise U = R
turnClockwise R = D

nextPoint :: Maze -> Coordinate -> Direction -> (Coordinate, Direction)
nextPoint maze point direction | maze `at` straightOn /= ' ' = (straightOn, direction)
                               | maze `at` clockwise /= ' ' = (clockwise, turnClockwise direction)
                               | maze `at` anticlockwise /= ' ' = (anticlockwise, turnAnticlockwise direction)
                               | otherwise = (straightOn, direction)
  where straightOn = point `move` direction
        clockwise = point `move` (turnClockwise direction)
        anticlockwise = point `move` (turnAnticlockwise direction)

getSequence :: Maze -> Coordinate -> [Char]
getSequence maze entry = getSequence' maze (entry, D) []
  where getSequence' maze (point, direction) sequence | current == ' ' = reverse sequence
                                                      | current >= 'A' && current <= 'Z' = getSequence' maze next (current:sequence)
                                                      | otherwise = getSequence' maze next sequence
           where current = maze `at` point
                 next = nextPoint maze point direction

main = do
  input <- lines <$> getContents
  let entry = findEntry input

  -- Part A
  print $ getSequence input entry
