import Data.String.Utils (join)
import Data.Maybe (fromJust)
import Data.Char (intToDigit)
import Data.List (findIndex, delete)
import Data.Bits (xor)
import qualified Data.Set as S
import Numeric (readHex, showHex, showIntAtBase)

parseInt :: Char -> Int
parseInt x = read [x] :: Int

parseInput :: [Char] -> [Int]
parseInput x = map fromEnum x

rotate :: Int -> [a] -> [a]
rotate x list = rotate' ((x + l) `mod` l) list
  where l = length list
        rotate' n list = (drop n list) ++ (take n list)

twist :: Int -> Int -> [Int] -> [Int]
twist index len values = rotate (-index) $ reverseFirst len $ rotate index values
  where reverseFirst n list = (reverse $ take n list) ++ (drop n list)

hash :: ([Int], [Int], Int, Int) -> ([Int], Int, Int)
hash ([], values, index, step) = (values, index, step)
hash ((l:ls), values, index, step) = hash (ls, (twist index l values), (index + l + step) , step + 1)

sparseHash :: [Int] -> [Int]
sparseHash ls = sparseHash' 64 ls ([0..255], 0, 0)
  where sparseHash' 0 _ (values, _, _) = values
        sparseHash' x ls (values, index, step) = sparseHash' (x-1) ls (hash (ls, values, index, step))

denseHash :: [Int] -> [Int]
denseHash [] = []
denseHash xs = (foldl xor 0 (take 16 xs)):(denseHash $ drop 16 xs)

fill :: Int -> a -> [a] -> [a]
fill n f x | length x < n = fill n f (f:x)
           | otherwise = x

knotHash :: [Char] -> [Char]
knotHash x = join "" $ map (fill 2 '0') $ map (\x -> showHex x "") $ denseHash $ sparseHash y
  where y = parseInput x ++ [17, 31, 73, 47, 23]

toBits :: [Char] -> [Int]
toBits x = map parseInt $ concat $ map (fill 4 '0') $ map (\y -> showIntAtBase 2 intToDigit (fst $ head $ readHex [y]) "") x

showBitArray :: [[Int]] -> [Char]
showBitArray array = join "\n" $ map (concat . map show) array

neighbours :: (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
neighbours (x, y) s = S.intersection s $ S.fromList [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

region :: (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
region point s = region' (S.empty) (S.singleton point) s
  where region' reg ps s | ps == S.empty = reg
                         | otherwise = region' newReg newPS s
          where newReg = S.union reg ps
                newPS = S.difference (S.unions $ map (\x -> neighbours x s) $ S.toList ps) newReg

removeRegion :: S.Set (Int, Int) -> S.Set (Int, Int)
removeRegion s | s == S.empty = S.empty
               | otherwise = S.difference s (region entry s)
  where entry = S.elemAt 0 s

countRegions :: S.Set (Int, Int) -> Int
countRegions set | set == S.empty = 0
                 | otherwise = 1 + (countRegions $ removeRegion set)

constructSet :: [[Int]] -> S.Set (Int, Int)
constructSet array = S.fromList $ [ (i, j) | (i, row) <- zip [0..] array, (j, el) <- zip [0..] row, el /= 0 ]

main = do
  input <- head <$> lines <$> getContents

  -- Part A
  let bitArray = map toBits $ map knotHash [ input ++ "-" ++ (show i) | i <- [0..127] ]
  print $ sum $ map sum bitArray

  -- Part B
  print $ countRegions $ constructSet bitArray
