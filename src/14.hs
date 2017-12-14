module DenseHash where

import Data.String.Utils
import Data.Char
import Data.Bits (xor)
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

fill :: [Char] -> [Char]
fill (x:[]) = '0':x:[]
fill x = x

knotHash :: [Char] -> [Char]
knotHash x = join "" $ map fill $ map (\x -> showHex x "") $ denseHash $ sparseHash y
  where y = parseInput x ++ [17, 31, 73, 47, 23]

toBits :: [Char] -> [Int]
toBits x = map parseInt $ showIntAtBase 2 intToDigit n ""
  where n = fst $ head $ readHex x

main = do
  input <- head <$> lines <$> getContents

  -- Part A
  print $ sum $ map sum $ map toBits $ map knotHash [ input ++ "-" ++ (show i) | i <- [0..127] ]
