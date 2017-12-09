
removeIgnoredCharacters :: [Char] -> [Char]
removeIgnoredCharacters [] = []
removeIgnoredCharacters ('!':y:xs) = removeIgnoredCharacters xs
removeIgnoredCharacters (x:xs) = [x] ++ removeIgnoredCharacters xs

removeGarbage :: [Char] -> [Char]
removeGarbage x = removeGarbage' False x
  where removeGarbage' _ [] = []
        removeGarbage' False ('<':xs) = removeGarbage' True xs
        removeGarbage' False (x:xs) = [x] ++ removeGarbage' False xs
        removeGarbage' True ('>':xs) = removeGarbage' False xs
        removeGarbage' True (x:xs) = removeGarbage' True xs

removeBoringCharacters :: [Char] -> [Char]
removeBoringCharacters x = [ y | y <- x, y == '{' || y == '}' ]

score :: [Char] -> Int
score x = score' 0 x
  where score' _ [] = 0
        score' depth ('{':xs) = score' (depth + 1) xs
        score' depth ('}':xs) = depth + (score' (depth - 1) xs)

main = do
  input <- head <$> lines <$> getContents

  -- Part A
  print $ score $ removeBoringCharacters $ removeGarbage $ removeIgnoredCharacters input
