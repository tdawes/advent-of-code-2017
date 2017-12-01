comparedElement :: (Num a) => [a] -> Int -> Int
comparedElement list index = (index + 1) `mod` (length list)

parseInput :: Char -> Int
parseInput x = read [x] :: Int

flatten :: [String] -> String
flatten [] = ""
flatten ([]:xss) = flatten xss
flatten ((x:xs):xss) = [x] ++ flatten (xs:xss)

listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

main = do
  numbers <- map parseInput <$> flatten <$> lines <$> getContents

  let matched = [ if (numbers!!x == numbers!!(comparedElement numbers x)) then numbers!!x else 0 | x <- [0..((length numbers) - 1)] ]

  print (listSum matched)
