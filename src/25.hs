import Data.String.Utils (split)
import qualified Data.Map as M
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (fromJust)

type Machine = (Int, M.Map Char Command)
type Command = Int -> (Int, Int, Char)

parseInt :: [Char] -> Int
parseInt x = read x :: Int

parseHeader :: [Char] -> Int
parseHeader header = parseInt $ (!!5) $ words $ (!!1) $ lines header

parseWrite :: [Char] -> Int
parseWrite = parseInt . (:[]) . head . (!!4) . words

parseMove :: [Char] -> Int
parseMove line = if ((!!6) $ words line) == "left." then -1 else 1

parseNext :: [Char] -> Char
parseNext = head . (!!4) . words

parseCommand :: [[Char]] -> Command
parseCommand body = command
  where parseCommand' [write, move, next] = (parseWrite write, parseMove move, parseNext next)
        onZero = parseCommand' $ take 3 $ drop 1 body
        onOne = parseCommand' $ drop 5 body
        command current = if current == 0 then onZero else onOne

parseState :: [[Char]] -> (Char, Command)
parseState (intro:body) = (head stateName, command)
  where stateMatcher = mkRegex "^In state ([A-Z]):$"
        stateName = head $ fromJust $ matchRegex stateMatcher intro
        command = parseCommand body

parseInput :: [[Char]] -> Machine
parseInput (header:states) = (parseHeader header, M.fromList $ map (parseState . lines) [ s | s <- states, s /= "" ])

safeGet :: Int -> M.Map Int Int -> Int
safeGet pos reg | pos `M.member` reg = reg M.! pos
                | otherwise = 0

step :: M.Map Char Command -> (Int, M.Map Int Int, Char) -> (Int, M.Map Int Int, Char)
step states (pos, reg, currentState) = (pos + newPos, M.insert pos newVal reg, nextState)
  where (newVal, newPos, nextState) = (states M.! currentState) (safeGet pos reg)

main = do
  (numSteps, states) <- parseInput <$> split "\n\n" <$> getContents

  -- Part A
  print $ length $ filter (\(_, v) -> v == 1) $ M.toList $ (\(_, reg, _) -> reg) $ foldl (\context _ -> step states context) (0, M.empty, 'A') [1..numSteps]
