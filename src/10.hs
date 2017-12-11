import Data.String.Utils
import Data.Char
import Data.Bits (xor)
import Numeric (showHex)

parseInput :: [Char] -> Int
parseInput x = read x :: Int

parseInput2 :: [Char] -> [Int]
parseInput2 x = map fromEnum x

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

main = do
  inputRaw <- head <$> lines <$> getContents
  let input = map parseInput $ split "," inputRaw

  -- Part A
  let (hashed, _, _) = hash (input, [0..255], 0, 0)
  print $ (hashed !! 0) * (hashed !! 1)

  let input2 = parseInput2 inputRaw ++ [17, 31, 73, 47, 23]
  print $ join "" $ map fill $ map (\x -> showHex x "") $ denseHash $ sparseHash input2
