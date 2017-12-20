import Data.String.Utils (split)
import Data.List (minimumBy)
import qualified Data.Set as S
import Data.List.Unique (isUnique)
import Data.Maybe (fromJust)

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

coordSum :: Coordinate -> Coordinate -> Coordinate
coordSum (a, b, c) (x, y, z) = (a + x, b + y, c + z)

updateData :: Data -> Data
updateData d = Data { index = (index d), position = newPosition, velocity = newVelocity, acceleration = newAcceleration }
  where newAcceleration = acceleration d
        newVelocity = coordSum (velocity d) newAcceleration
        newPosition = coordSum (position d) newVelocity

update :: [Data] -> [Data]
update datas = [ d | d <- newDatas, fromJust $ isUnique (position d) newPositions ]
  where newDatas = map updateData datas
        newPositions = map position newDatas

main = do
  input <- map parseInput <$> zip [0..] <$> lines <$> getContents

  -- Part A
  print $ index $ minimumBy (\x y -> compareCoords (acceleration x) (acceleration y) ) input

  -- Part B
  print $ length $ (!!10000) $ iterate update input
