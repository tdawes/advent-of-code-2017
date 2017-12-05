import Data.List
import Data.Maybe
import Data.Function.Memoize


type Coord = (Int, Int)
type Element = (Int, Coord)

parseInput :: [Char] -> Int
parseInput x = read x :: Int

plus :: Coord -> Coord -> Coord
plus (x, y) (a, b) = (x + a, y + b)

minus :: Coord -> Coord -> Coord
minus (x, y) (a, b) = (x - a, y - b)

corners :: [Element]
corners = [(0, (0,0))]
  ++ (concat [ [ (s1, (i, i-1)),
                 (s1 + i1, (i,-i)),
                 (s2, (-i, -i)),
                 (s2+i2, (-i, i)) ] | i <- [1..],
                                         let i1 = 2*i - 1,
                                         let s1 = i1^2,
                                         let i2 = 2*i,
                                         let s2 = i2^2 ] )

nextCorner :: Int -> Element
nextCorner x = head [ corner | corner <- corners, fst corner > x ]

previousCorner :: Int -> Element
previousCorner x = corners !! ((fromJust (elemIndex (nextCorner x) corners)) - 1)

interp :: (RealFrac a) => Int -> Int -> a -> Int
interp x y l = round $ ((1-l) * xx) + (l * yy)
  where xx = fromIntegral x
        yy = fromIntegral y

interpCoord :: (RealFrac a) => Coord -> Coord -> a -> Coord
interpCoord (x, y) (a, b) l = (interp x a l, interp y b l)

getCoordinate :: Int -> Coord
getCoordinate i = interpCoord (snd p) (snd n) l
  where p = previousCorner i
        n = nextCorner i
        l = (fromIntegral $ i - (fst p)) / (fromIntegral $ (fst n) - (fst p))

spiral = [ (i, getCoordinate i) | i <- [0..] ]

neighbours :: Coord -> [Coord]
neighbours (x, y) = [ (x + c, y + d) | c <- [-1..1], d <- [-1..1], c /= 0 || d /= 0 ]

value :: Int -> Int
value' 0 = 1
value' x = sum [ value i | (i, n) <- take x spiral, elem n (neighbours $ getCoordinate x) ]
value = memoize value'

main = do
  input <- parseInput <$> getContents

  -- Part A
  let coord = getCoordinate (input - 1)
  print $ (abs $ fst coord) + (abs $ snd coord)

  -- Part B
  print $ head [ v | x <- [0..], let v = value x, v >= input ]
