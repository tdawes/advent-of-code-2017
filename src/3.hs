import Data.Function.Memoize

type Coord = (Int, Int)
type Element = (Int, Coord)

parseInput :: [Char] -> Int
parseInput x = read x :: Int

shift :: Coord -> Coord -> Coord
shift (x, y) (a, b) = (x + a, y + b)

corners :: [Element]
corners = [ (index, (i-1, i-1)) | i <- [1..], let index = (2*i - 1)^2 ]

nextCorner :: Int -> Element
nextCorner index = head [ corner | corner <- corners, fst corner >= index ]

getCoordinate :: Int -> Coord
getCoordinate i | i == s = c
                | i >= s - d = (l - (s - i), l)
                | i >= s - (2*d) = ((-l), l - (s - d - i))
                | i >= s - (3*d) = ((s - (2*d) - i) - l, (-l))
                | otherwise = (l, (s - (3*d) - i) - l)
  where (s, c) = nextCorner i
        l = fst c
        d = 2*l

spiral = [ (i, getCoordinate i) | i <- [1..] ]

neighbours :: Coord -> [Coord]
neighbours (x, y) = [ (x + c, y + d) | c <- [-1..1], d <- [-1..1], c /= 0 || d /= 0 ]

value :: Int -> Int
value' 1 = 1
value' x = sum [ value i | (i, n) <- take (x-1) spiral, elem n (neighbours $ getCoordinate x) ]
value = memoize value'

main = do
  input <- parseInput <$> getContents

  -- Part A
  let coord = getCoordinate input
  print $ (abs $ fst coord) + (abs $ snd coord)

  -- Part B
  print $ head [ v | x <- [1..], let v = value x, v >= input ]
