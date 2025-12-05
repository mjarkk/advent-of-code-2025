module Main where

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

parse :: String -> ([(Int, Int)], [Int])
parse input =
  let (freshRangesInput, availableIngredientsInput) = splitOnceAt input (indexOf "\n\n" input) 2
      freshRanges = map (\(a, b) -> (read a, read b)) $ map (\line -> splitOnceAt line (indexOf "-" line) 1) $ lines freshRangesInput
      availableIngredients = map read $ filter (/= "") $ lines availableIngredientsInput
   in (freshRanges, availableIngredients)

splitOnceAt :: String -> Int -> Int -> (String, String)
splitOnceAt input index dropN = (take index input, drop (index + dropN) input)

indexOf :: String -> String -> Int
indexOf _ "" = -1
indexOf needle haystack =
  let needleLen = length needle
   in if take needleLen haystack == needle
        then 0
        else 1 + (indexOf needle $ tail haystack)

solve :: String -> (Int, Int)
solve input =
  let (freshRanges, availableIngredients) = parse input
   in ( sum $ map (\x -> if inRange x freshRanges then 1 else 0) availableIngredients,
        solveP2 freshRanges
      )

inRange :: Int -> [(Int, Int)] -> Bool
inRange x ranges = any (\(a, b) -> a <= x && x <= b) ranges

solveP2 :: [(Int, Int)] -> Int
solveP2 ranges =
  let filteredRanges = filter (\rn -> not $ rangeWithinOtherRange rn ranges) ranges
      subStract = foldl (\acc (rn, toDrop) -> acc + countOverlap rn (drop toDrop filteredRanges)) 0 $ zip filteredRanges [1 ..]
   in foldl (\acc (start, end) -> acc + (end - start) + 1) (-subStract) filteredRanges

countOverlap :: (Int, Int) -> [(Int, Int)] -> Int
countOverlap (start, end) ranges =
  let (overlapLeft, overlapRight) = countOverlapInner (start, end) ranges
   in min (overlapLeft + overlapRight) ((end - start) + 1)

countOverlapInner :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
countOverlapInner _ [] = (0, 0)
countOverlapInner (startA, endA) ((startB, endB) : rest) =
  let (remLeftOverlap, remRightOverlap) = countOverlapInner (startA, endA) rest
      overlap = max 0 (min endA endB - max startA startB + 1)
      isLeft = startA >= startB && endA >= endB
   in if isLeft
        then (max remLeftOverlap overlap, remRightOverlap)
        else (remLeftOverlap, max remRightOverlap overlap)

rangeWithinOtherRange :: (Int, Int) -> [(Int, Int)] -> Bool
rangeWithinOtherRange _ [] = False
rangeWithinOtherRange (startA, endA) ((startB, endB) : rest) = (startA /= startB || endA /= endB) && startA >= startB && endA <= endB && rangeWithinOtherRange (startA, endA) rest
