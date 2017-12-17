import Data.List (elemIndex)

parseInt :: [Char] -> Int
parseInt x = read x :: Int

twist :: Int -> [a] -> [a]
twist n list = (drop m list) ++ (take m list)
  where m = n `mod` (length list)

spinlock :: Int -> Int -> [Int]
spinlock turns step = spinlock' [0] turns step
  where spinlock' buffer 0 _ = buffer
        spinlock' buffer t s = spinlock' ((turns + 1 - t):(twist (s+1) buffer)) (t-1) s


main = do
  input <- parseInt <$> head <$> lines <$> getContents

  -- Part A
  print $ (spinlock 2017 input) !! 1
