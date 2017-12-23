import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Numbers.Primes (isPrime)

-- (Register, Position, Count)
type Context = (M.Map [Char] Int, Int, Int)
type Command = Context -> Context

parseInt :: [Char] -> Int
parseInt x = read x :: Int

value :: M.Map [Char] Int -> [Char] -> Int
value register k | 'a' <= (head k) && (head k) <= 'z' = M.findWithDefault 0 k register
                 | otherwise = parseInt k

withDefault :: a -> (a -> a) -> Maybe a -> Maybe a
withDefault def f Nothing = Just (f def)
withDefault def f (Just x) = Just (f x)

parseInput :: [[Char]] -> Command
parseInput ["set", x, y] (register, pos, count) = (M.insert x (value register y) register, pos + 1, count)
parseInput ["sub", x, y] (register, pos, count) = (M.alter (withDefault 0 (\a -> a - (value register y))) x register, pos + 1, count)
parseInput ["mul", x, y] (register, pos, count) = (M.alter (withDefault 0 (\a -> a * (value register y))) x register, pos + 1, count + 1)
parseInput ["jnz", x, y] (register, pos, count) = (register, if (value register x) /= 0 then pos + (value register y) else pos + 1, count)

run :: [Command] -> Context -> Context
run commands = run'
  where run' (register, pos, count) | pos < 0 || pos >= (length commands) = (register, pos, count)
                                    | otherwise = run' $ (commands !! pos) (register, pos, count)

main = do
  input <- map parseInput <$> map words <$> lines <$> getContents

  -- Part A
  let (_, _, count) = run input (M.empty, 0, 0)
  print count

  -- Part B
  let b = (79 * 100) + 100000
  let c = b + 17000
  print $ foldl (\count n -> if isPrime (b + (17 * n)) then count else count + 1) 0 [0..1000]
