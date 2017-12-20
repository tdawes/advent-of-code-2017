import Data.String.Utils (split)
import Data.List (minimumBy)

type Coordinate = (Int, Int, Int)
data Data = Data { index :: Int
                 , position :: Coordinate
                 , velocity :: Coordinate
                 , acceleration :: Coordinate
                 } deriving (Show)

parseInt :: [Char] -> Int
parseInt x = read x :: Int

parseCoordinate :: [Char] -> Coordinate
parseCoordinate str = (parts !! 0, parts !! 1, parts !! 2)
  where parts = map parseInt $ split "," str

parseInput :: (Int, [Char]) -> Data
parseInput (index, line) = Data { index = index, position = (parseCoordinate $ parts !! 1), velocity = (parseCoordinate $ parts !! 3), acceleration = (parseCoordinate $ parts !! 5) }
  where parts = concat $ map (split "<") $ split ">" line

compareCoords :: Coordinate -> Coordinate -> Ordering
compareCoords (a, b, c) (x, y, z) = compare ((abs a) + (abs b) + (abs c)) ((abs x) + (abs y) + (abs z))

main = do
  input <- map parseInput <$> zip [0..] <$> lines <$> getContents

  -- Part A
  print $ index $ minimumBy (\x y -> compareCoords (acceleration x) (acceleration y) ) input
