import Data.Char (intToDigit)
import Numeric (readHex, showIntAtBase)
import Control.Monad.ST
import Data.STRef
import Control.Monad.Loops
import Control.Monad

parseInt :: [Char] -> Int
parseInt x = read x :: Int

parseInput :: [Char] -> Int
parseInput x = parseInt $ words x !! 4

fill :: Int -> a -> [a] -> [a]
fill n f x | length x < n = fill n f (f:x)
           | otherwise = x

hashA :: Int -> Int
hashA x = (16807 * x) `rem` 2147483647

hashB :: Int -> Int
hashB x = (48271 * x) `rem` 2147483647

comp :: Int -> Int -> Bool
comp x y = bits x == bits y
  where bits z = take 16 $ reverse $ fill 16 '0' $ showIntAtBase 2 intToDigit z ""

countMatches :: Int -> Int -> (Int -> Bool) -> Int -> (Int -> Bool) -> Int
countMatches n a pa b pb = runST $ do
  c <- newSTRef 0
  stA <- newSTRef a
  stB <- newSTRef b

  forM_ [0..n] $ \_ -> do
      iterateWhile (not . pa) (do
                                aa <- hashA <$> readSTRef stA
                                writeSTRef stA aa
                                return aa
                              )
      iterateWhile (not . pb) (do
                                bb <- hashB <$> readSTRef stB
                                writeSTRef stB bb
                                return bb
                              )

      aa <- readSTRef stA
      bb <- readSTRef stB
      when (comp aa bb) $ do
        modifySTRef c (+1)

  readSTRef c

main = do
  [a, b] <- map parseInput <$> lines <$> getContents

  -- Part A
  print $ countMatches 40000000 a (\_ -> True) b (\_ -> True)

  -- Part B
  print $ countMatches 5000000 a (\x -> x `rem` 4 == 0) b (\x -> x `rem` 8 == 0)
