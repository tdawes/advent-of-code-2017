flatten :: [String] -> String
flatten [] = ""
flatten ([]:xss) = flatten xss
flatten ((x:xs):xss) = [x] ++ flatten (xs:xss)

parseInput :: Char -> Int
parseInput x = read [x] :: Int

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n list = drop m list ++ take m list
  where m = n `mod` (length list)

main = do
  numbers <- map parseInput <$> flatten <$> lines <$> getContents

  -- Part A
  print $ sum [ x | (x, y) <- zip numbers (rotate 1 numbers), x == y ]

  -- Part B
  print $ sum [ x | (x, y) <- zip numbers (rotate (quot (length numbers) 2) numbers), x == y ]
  
