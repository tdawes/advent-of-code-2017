import qualified Data.Map as M
import Data.Maybe (fromJust)

-- (Register, Stack, Position, Output)
type Context = (M.Map [Char] Int, [Int], Int, [Int])
type Command = Context -> Context

-- (Register, Output, Default, Position, Count, DidRead, Waiting)
type Context2 = (M.Map [Char] Int, [Int], Int, Int, Int, Bool, Bool)
type Command2 = Context2 -> [Int] -> Context2

parseInt :: [Char] -> Int
parseInt x = read x :: Int

value :: M.Map [Char] Int -> [Char] -> Int
value register k | 'a' <= (head k) && (head k) <= 'z' = M.findWithDefault 0 k register
                 | otherwise = parseInt k

value2 :: Int -> M.Map [Char] Int -> [Char] -> Int
value2 def register k | 'a' <= (head k) && (head k) <= 'z' = M.findWithDefault def k register
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

parseInput2 :: [[Char]] -> Command2
parseInput2 ["set", x, y] (register, output, def, pos, count, _, _) _ = (M.insert x (value2 def register y) register, output, def, pos + 1, count, False, False)
parseInput2 ["add", x, y] (register, output, def, pos, count, _, _) _ = (M.alter (withDefault def (\a -> a + (value2 def register y))) x register, output, def, pos + 1, count, False, False)
parseInput2 ["mul", x, y] (register, output, def, pos, count, _, _) _ = (M.alter (withDefault def (\a -> a * (value2 def register y))) x register, output, def, pos + 1, count, False, False)
parseInput2 ["mod", x, y] (register, output, def, pos, count, _, _) _ = (M.alter (withDefault def (\a -> a `mod` (value2 def register y))) x register, output, def, pos + 1, count, False, False)
parseInput2 ["jgz", x, y] (register, output, def, pos, count, _, _) _ = (register, output, def, if (value2 def register x) > 0 then pos + (value2 def register y) else pos + 1, count, False, False)
parseInput2 ["snd", x] (register, output, def, pos, count, _, _) _ = (register, output ++ [value2 def register x], def, pos + 1, count + 1, False, False)
parseInput2 ["rcv", x] (register, output, def, pos, count, _, _) [] = (register, output, def, pos, count, False, True)
parseInput2 ["rcv", x] (register, output, def, pos, count, _, _) (i:_) = (M.insert x i register, output, def, pos + 1, count, True, False)

run :: [Command] -> Context -> Context
run commands = run'
  where run' (register, stack, pos, sounds) | sounds /= [] = (register, stack, pos, sounds)
                                            | pos < 0 || pos >= (length commands) = (register, stack, pos, sounds)
                                            | otherwise = run' $ (commands !! pos) (register, stack, pos, sounds)

run2 :: [Command2] -> (Context2, Context2) -> (Context2, Context2)
run2 commands = run2'
  where run2' (context1, context2) | (deadlock1 || pos1 < 0 || pos1 >= (length commands)) && (deadlock2 || pos2 < 0 || pos2 >= (length commands)) = (context1, context2)
                                   | otherwise = run2' ((commands !! pos1) (register1, output1, def1, pos1, count1, didRead1, deadlock1) input1, (commands !! pos2) (register2, output2, def2, pos2, count2, didRead2, deadlock2) input2)
          where (register1, o1, def1, pos1, count1, didRead1, deadlock1) = context1
                (register2, o2, def2, pos2, count2, didRead2, deadlock2) = context2
                output1 = if didRead2 then tail o1 else o1
                output2 = if didRead1 then tail o2 else o2
                input1 = output2
                input2 = output1

main = do
  input <- map words <$> lines <$> getContents

  -- Part A
  let commands = map parseInput input
  let (_, _, _, sounds) = run commands (M.empty, [], 0, [])
  print $ head $ reverse sounds

  -- Part B
  let commands2 = map parseInput2 input
  let (_, (_, _, _, _, count, _, _)) = run2 commands2 ((M.empty, [], 0, 0, 0, False, False), (M.empty, [], 1, 0, 0, False, False))
  print count
