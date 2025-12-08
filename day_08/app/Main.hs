module Main where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Debug.Trace
import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  puzzleContents <- readFile "puzzle.txt"
  print $ solve puzzleContents
  end <- getCPUTime
  let diff = (fromIntegral (end - start) / 1000000000 :: Double)
  putStrLn ("Execution time: " ++ show diff ++ " ms")

parse :: String -> [(Int, Int, Int)]
parse input = map (\[x, y, z] -> (read x, read y, read z)) $ map (\l -> split l ',') $ filter (/= "") $ lines input

split :: String -> Char -> [String]
split "" _ = [""]
split (x : xs) delimiter
  | x == delimiter = "" : split xs delimiter
  | otherwise =
      let result = split xs delimiter
       in (x : result !! 0) : drop 1 result

solve :: String -> (Int, Int)
solve puzzle =
  let input = parse puzzle
   in (solve2 input, 0)
   -- in (length $ traceShowId $ takeUniqueGroups $ addGroups (zip input $ replicate (length input) (-1)) 0, 0)

takeUniqueGroups :: [((Int, Int, Int), Int)] -> [[(Int, Int, Int)]]
takeUniqueGroups groups =
  let maxGroupId = maximum $ map snd groups
      response = foldl (\acc (point, group) -> replaceIdx (group - 1) (point : (acc !! (group - 1))) acc) (replicate maxGroupId ([] :: [(Int, Int, Int)])) groups
   in filter (\l -> length l > 0) response

solve2 :: [(Int, Int, Int)] -> Int
solve2 input =
  let result = map (\(a, b, _) -> (a, b)) take 10 $ sortBy (\(_, _, a) (_, _, b) -> compare a b) $ map (\(a, b) -> (a, b, straightLineDistance a b)) $ allPairs input
   in length $ traceShowId result

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x:xs) = [(x, y) | y <- xs] ++ allPairs xs

addGroups :: [((Int, Int, Int), Int)] -> Int -> [((Int, Int, Int), Int)]
addGroups points indexToCheck
  | indexToCheck >= length points = points
  | snd (points !! indexToCheck) /= -1 = addGroups points (indexToCheck + 1)
  | otherwise =
      let point = fst (points !! indexToCheck)
          foldFn (closest, accIndex) ((needle, _), needleIndex) =
            let distance = straightLineDistance needle point
             in if indexToCheck /= needleIndex && distance < closest then (distance, needleIndex) else (closest, accIndex)
          closestIdx = snd $ foldl foldFn (99999999 :: Double, 0) $ zip points [0 ..]
       in addGroups (addToGroup points indexToCheck closestIdx) (indexToCheck + 1)

addToGroup :: [((Int, Int, Int), Int)] -> Int -> Int -> [((Int, Int, Int), Int)]
addToGroup points idx1 idx2
  | group1 == (-1) && group2 == (-1) -- add new group
    =
      let maxGroupNr = maximum $ map snd points
          groupNr = (max maxGroupNr 0) + 1
       in replaceIdx idx2 (point2, groupNr) $ replaceIdx idx1 (point1, groupNr) points
  | group1 == (-1) = replaceIdx idx1 (point1, group2) points -- add group 2 to group 1
  | group2 == (-1) = replaceIdx idx2 (point2, group1) points -- add group 1 to group 2
  | group1 == group2 = points -- do nothing
  | group1 /= group2 = map (\(point, group) -> if group == group1 then (point, group2) else (point, group)) points -- merge groups
  | otherwise = error "This should never happen"
  where
    point1 = fst (points !! idx1)
    group1 = snd (points !! idx1)
    point2 = fst (points !! idx2)
    group2 = snd (points !! idx2)

replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

straightLineDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
straightLineDistance (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2
