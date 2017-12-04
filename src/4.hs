import qualified Data.Set as Set
import Data.List

isValid :: [[Char]] -> Bool
isValid list = length list == length set
  where set = Set.fromList $ map sort list

main = do
  lines <- map words <$> lines <$> getContents
  let validLines = [ line | line <- lines, isValid line ]
  print $ length validLines
