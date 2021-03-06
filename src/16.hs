import Data.String.Utils (split)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

parseInt :: [Char] -> Int
parseInt x = read x :: Int

spin :: Int -> [a] -> [a]
spin n list = (drop m list) ++ (take m list)
  where m = (length list) - (n `mod` (length list))

swap :: Int -> Int -> [a] -> [a]
swap a b list | b < a = swap b a list
              | otherwise = (take a list) ++ [list !! b] ++ (take (b-a-1) $ drop (a+1) list) ++ [list !! a] ++ (drop (b+1) list)

swapElem :: (Eq a) => a -> a -> [a] -> [a]
swapElem a b list = swap (fromJust $ elemIndex a list) (fromJust $ elemIndex b list) list

parseInput :: [Char] -> [Char] -> [Char]
parseInput ('s':r) = spin $ parseInt r
parseInput ('x':r) = swap (parseInt a) (parseInt b)
  where [a,b] = split "/" r
parseInput ('p':r) = swapElem (head a) (head b)
  where [a,b] = split "/" r
parseInput x = error x

doTheDance :: [[Char] -> [Char]] -> [Char] -> [Char]
doTheDance steps positions = foldl (\ps step -> step ps) positions steps

doTheDanceUntilLoop :: [[Char] -> [Char]] -> [Char] -> [[Char]]
doTheDanceUntilLoop steps start = dance steps [] start
  where dance steps previous positions | positions `elem` previous = previous
                                       | otherwise = dance steps (positions:previous) (doTheDance steps positions)

main = do
  input <- map parseInput <$> split "," <$> head <$> lines <$> getContents

  -- Part A
  let start = ['a'..'p']
  let final = doTheDance input start
  print final

  -- Part B
  let loop = doTheDanceUntilLoop input ['a'..'p']
  print $ loop !! (length loop - 1 - (1000000000 `mod` (length loop)))
