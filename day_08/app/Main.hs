module Main where

import Data.List (sort, sortBy)
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

takeUniqueGroups :: [((Int, Int, Int), Int)] -> [[(Int, Int, Int)]]
takeUniqueGroups groups =
  let maxGroupId = maximum $ map snd groups
      response = foldl (\acc (point, group) -> replaceIdx (group - 1) (point : (acc !! (group - 1))) acc) (replicate maxGroupId ([] :: [(Int, Int, Int)])) groups
   in filter (\l -> length l > 0) response

solve :: String -> (Int, Int)
solve puzzle =
  let input = parse puzzle
      takeN = if length input > 100 then 1000 else 10
      setsSortedByConnectionLen = sortBy (\(_, _, a) (_, _, b) -> compare a b) $ map (\(a, b) -> (a, b, straightLineDistance a b)) $ allPairs input
      (sets, _, _) = foldl (\acc (a, b, _) -> createSets acc (a, b)) ([], (0, 0, 0), (0, 0, 0)) $ take takeN setsSortedByConnectionLen
      resultP1 = foldl (\acc item -> item * acc) 1 $ take 3 $ reverse $ sort $ map length sets
      (_, (xa, _, _), (xb, _, _)) = foldl (\acc (a, b, _) -> createSets acc (a, b)) ([], (0, 0, 0), (0, 0, 0)) setsSortedByConnectionLen
   in (resultP1, xa * xb)

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = [(x, y) | y <- xs] ++ allPairs xs

createSets :: ([[(Int, Int, Int)]], (Int, Int, Int), (Int, Int, Int)) -> ((Int, Int, Int), (Int, Int, Int)) -> ([[(Int, Int, Int)]], (Int, Int, Int), (Int, Int, Int))
createSets ([], lastA, lastB) (a, b) = ([[a, b]], lastA, lastB)
createSets (acc, lastA, lastB) (a, b)
  | aIdx == (-1) && bIdx == (-1) = ([a, b] : acc, lastA, lastB)
  | aIdx == (-1) = (replaceIdx bIdx (a : (acc !! bIdx)) acc, a, b)
  | bIdx == (-1) = (replaceIdx aIdx (b : (acc !! aIdx)) acc, a, b)
  | aIdx == bIdx = (acc, lastA, lastB)
  | aIdx /= bIdx =
      let newList = (acc !! aIdx) ++ (acc !! bIdx)
       in (newList : removeIdxes [aIdx, bIdx] acc, a, b)
  | otherwise = error "wut?"
  where
    aIdx = setLocation acc a
    bIdx = setLocation acc b

setLocation :: [[(Int, Int, Int)]] -> (Int, Int, Int) -> Int
setLocation baseSets needle = foldl (\acc (subSet, idx) -> if needle `elem` subSet then idx else acc) (-1) $ zip baseSets [0 ..]

replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

removeIdxes :: [Int] -> [a] -> [a]
removeIdxes [] xs = xs
removeIdxes indexes xs =
  let sortedIndexes = reverse $ sort indexes
      firstIdx = sortedIndexes !! 0
   in removeIdxes (drop 1 sortedIndexes) (take firstIdx xs ++ drop (firstIdx + 1) xs)

straightLineDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
straightLineDistance (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2
