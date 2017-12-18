import qualified Data.Map as M
import Data.Maybe (fromJust)

type Context = (M.Map [Char] Int, [Int], Int, [Int])
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
parseInput ["set", x, y] (register, stack, pos, sounds) = (M.insert x (value register y) register, stack, pos + 1, sounds)
parseInput ["add", x, y] (register, stack, pos, sounds) = (M.alter (withDefault 0 (\a -> a + (value register y))) x register, stack, pos + 1, sounds)
parseInput ["mul", x, y] (register, stack, pos, sounds) = (M.alter (withDefault 0 (\a -> a * (value register y))) x register, stack, pos + 1, sounds)
parseInput ["mod", x, y] (register, stack, pos, sounds) = (M.alter (withDefault 0 (\a -> a `mod` (value register y))) x register, stack, pos + 1, sounds)
parseInput ["jgz", x, y] (register, stack, pos, sounds) = (register, stack, if (value register x) > 0 then pos + (value register y) else pos + 1, sounds)
parseInput ["snd", x] (register, stack, pos, sounds) = (register, (value register x):stack, pos + 1, sounds)
parseInput ["rcv", x] (register, stack, pos, sounds) = (register, if shouldJump then tail stack else stack, pos + 1, if shouldJump then (head stack):sounds else sounds)
  where shouldJump = (value register x) /= 0

run :: [Command] -> Context -> Context
run commands = run'
  where run' (register, stack, pos, sounds) | sounds /= [] = (register, stack, pos, sounds)
                                            | pos < 0 || pos >= (length commands) = (register, stack, pos, sounds)
                                            | otherwise = run' $ (commands !! pos) (register, stack, pos, sounds)

main = do
  input <- map parseInput <$> map words <$> lines <$> getContents

  -- Part A
  let (_, _, _, sounds) = run input (M.empty, [], 0, [])
  print $ head $ reverse sounds
