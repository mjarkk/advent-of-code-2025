module Main where

import Data.List (transpose)
import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  puzzleContents <- readFile "puzzle.txt"
  print $ solve puzzleContents
  end <- getCPUTime
  let diff = (fromIntegral (end - start) / 1000000000 :: Double)
  putStrLn ("Execution time: " ++ show diff ++ " ms")

parseP1 :: String -> ([[Int]], [Char])
parseP1 input =
  let inputLines = filter (/= "") $ lines input
      numbers = map (map read . takeWords) $ take (length inputLines - 1) inputLines
      operators = map (\w -> w !! 0) $ takeWords $ last inputLines
   in (transpose numbers, operators)

parseP2 :: String -> [String]
parseP2 input =
  let inputLines = filter (/= "") $ lines input
      longestLine = maximum $ map length inputLines
      sanitizedInputLines = map (\l -> l ++ replicate (longestLine - length l) ' ') inputLines
   in filter (/= "") $ map (concat . filter (/= "") . words) $ transpose sanitizedInputLines

takeWords :: String -> [String]
takeWords line = filter (/= "") $ words line

solve :: String -> (Int, Int)
solve input =
  let (numbers, operators) = parseP1 input
   in ( foldl (\acc (op, col) -> acc + solveCol col op) 0 $ zip operators numbers,
        takeP2Result $ foldl solveP2Col (0, 0, ' ') $ parseP2 input
      )

takeP2Result :: (Int, Int, Char) -> Int
takeP2Result (x, v, _) = x + v

solveCol :: [Int] -> Char -> Int
solveCol [] _ = 0
solveCol [x] _ = x
solveCol (x : xs) '+' = x + solveCol xs '+'
solveCol (x : xs) '*' = x * solveCol xs '*'
solveCol _ op = error ("Invalid operator " ++ [op])

solveP2Col :: (Int, Int, Char) -> String -> (Int, Int, Char)
solveP2Col (result, blockResult, op) item
  | startNewBlock = (result + blockResult, itemNum, lastItem)
  | otherwise = (result, if op == '+' then blockResult + itemNum else blockResult * itemNum, op)
  where
    lastItem = last item
    startNewBlock = lastItem == '+' || lastItem == '*'
    itemNum = read (if startNewBlock then take (length item - 1) item else item) :: Int
