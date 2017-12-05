import qualified Data.Set as Set
import Data.List

main = do
  lines <- map words <$> lines <$> getContents

  -- Part A
  print $ length [ line | line <- lines,
                          let set = Set.fromList line,
                          length line == length set]

  -- Part B
  print $ length [ line | line <- lines,
                          let set = Set.fromList $ map sort line,
                          length line == length set]
