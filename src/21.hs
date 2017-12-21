import Data.String.Utils (split)
import Data.List (nub, find, transpose)
import Data.Maybe (fromJust)

type Pattern = [[Char]]
type Rule = (Pattern, Pattern)

rotate :: Pattern -> Pattern
rotate [] = repeat []
rotate (r:rs) = map (\(x, y)-> x:y) $ zip (reverse r) $ rotate rs

permutations :: Pattern -> [Pattern]
permutations pattern = nub $ (take 4 $ iterate rotate pattern) ++ (take 4 $ iterate rotate $ reverse pattern)

parseInput :: [[Char]] -> Rule
parseInput [from, "=>", to] = (readPattern from, readPattern to)
  where readPattern x = split "/" x

part :: Int -> [a] -> [[a]]
part _ [] = []
part n list = (take n list):(part n $ drop n list)

cut :: Int -> Pattern -> [[Pattern]]
cut size pattern = map transpose $ map (map (part size)) $ part size pattern

stepPattern :: [Rule] -> Pattern -> Pattern
stepPattern rules pattern = snd $ fromJust $ find (\(match, _) -> elem pattern $ permutations match) rules

joinPatterns :: [[Pattern]] -> Pattern
joinPatterns = concat . (map joinPatterns')
  where joinPatterns' [] = repeat []
        joinPatterns' (p:ps) = map (\(x, y) -> x ++ y) $ zip p $ joinPatterns' ps

step :: [Rule] -> Pattern -> Pattern
step rules pattern | length pattern `mod` 2 == 0 = joinPatterns $ map (map (stepPattern rules)) $ cut 2 pattern
                   | length pattern `mod` 3 == 0 = joinPatterns $ map (map (stepPattern rules)) $ cut 3 pattern

main = do
  input <- map parseInput <$> map words <$> lines <$> getContents
  let initialPattern = [".#.", "..#", "###"]

  -- Part A
  print $ sum $ map sum $ map (map (\x -> if x == '#' then 1 else 0)) $ (!!5) $ iterate (step input) initialPattern

  -- Part B
  print $ sum $ map sum $ map (map (\x -> if x == '#' then 1 else 0)) $ (!!18) $ iterate (step input) initialPattern
