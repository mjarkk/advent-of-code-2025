module Main where

import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime

  puzzleContents <- readFile "puzzle.txt"
  print $ solve puzzleContents

  end <- getCPUTime
  let diff = round (fromIntegral (end - start) / 1000000000 :: Double)
  putStrLn ("Execution time: " ++ show diff ++ " ms")

parse :: String -> [[Int]]
parse input = map (\l -> map (\c -> read [c] :: Int) l) $ filter (/= "") $ lines input

solve :: String -> (Int, Int)
solve puzzleInput =
  let parsedPuzzle = parse puzzleInput
   in foldl (\(p1, p2) row -> (p1 + solveRow 2 row, p2 + solveRow 12 row)) (0, 0) parsedPuzzle

solveRow :: Int -> [Int] -> Int
solveRow n row =
  let solution = read $ concat $ map show $ fst $ foldl (\acc idx -> takeHighestNum acc idx n row) ([], 0) [0 .. n - 1]
   in solution

takeHighestNum :: ([Int], Int) -> Int -> Int -> [Int] -> ([Int], Int)
takeHighestNum (accList, accOffset) idx n row =
  let dropFromEnd = n - idx - 1
      searchSpace = drop accOffset $ take (length row - dropFromEnd) row
      (val, valIdx) = highestNumberAt searchSpace
   in (accList ++ [val], accOffset + valIdx + 1)

highestNumberAt :: [Int] -> (Int, Int) -- (val, idx)
highestNumberAt row = foldl (\(maxVal, maxIdx) (val, idx) -> if val > maxVal then (val, idx) else (maxVal, maxIdx)) (-1, -1) $ zip row [0 ..]
