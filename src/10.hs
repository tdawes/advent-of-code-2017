import Data.String.Utils

parseInput :: [Char] -> Int
parseInput x = read x :: Int

rotate :: Int -> [a] -> [a]
rotate x list = rotate' ((x + l) `mod` l) list
  where l = length list
        rotate' n list = (drop n list) ++ (take n list)

twist :: Int -> Int -> [Int] -> [Int]
twist index len values = rotate (-index) $ reverseFirst len $ rotate index values
  where reverseFirst n list = (reverse $ take n list) ++ (drop n list)

hash :: [Int] -> [Int]
hash list = hash' 0 0 [0..255] list
  where hash' step index values [] = values
        hash' step index values (l:ls) = hash' (step + 1) (index + l + step) (twist index l values) ls

main = do
  input <- map parseInput <$> split "," <$> head <$> lines <$> getContents

  -- Part A
  let hashed = hash input
  print $ (hashed !! 0) * (hashed !! 1)
