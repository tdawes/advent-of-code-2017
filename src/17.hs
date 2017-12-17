import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad.ST
import Data.STRef
import Control.Monad.Loops
import Control.Monad

parseInt :: [Char] -> Int
parseInt x = read x :: Int

twist :: Int -> [a] -> [a]
twist n list = (drop m list) ++ (take m list)
  where m = n `mod` (length list)

spinlock :: Int -> Int -> [Int]
spinlock turns step = spinlock' [0] turns step
  where spinlock' buffer 0 _ = buffer
        spinlock' buffer t s = spinlock' ((turns + 1 - t):(twist (s+1) buffer)) (t-1) s

afterZero :: Int -> Int -> Int
afterZero turns step = runST $ do
  currentPos <- newSTRef 0
  afterZ <- newSTRef 0

  forM_ [1..turns] $ \x -> do
    pos <- readSTRef currentPos
    let newPos = (pos + step) `mod` x
    writeSTRef currentPos (newPos + 1)
    when (newPos == 0) $ do
      writeSTRef afterZ x

  readSTRef afterZ

main = do
  input <- parseInt <$> head <$> lines <$> getContents

  -- Part A
  print $ (spinlock 2017 input) !! 1

  -- Part B
  print $ afterZero 50000000 input
