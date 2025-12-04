module Main where

import System.CPUTime
import Debug.Trace

main :: IO ()
main = do
  start <- getCPUTime
  puzzleContents <- readFile "puzzle.txt"
  print $ solve puzzleContents
  end <- getCPUTime
  let diff = round (fromIntegral (end - start) / 1000000000 :: Double)
  putStrLn ("Execution time: " ++ show diff ++ " ms")

solve :: String -> (Int, Int)
solve input =
  let warehouse = filter (/= "") $ lines input
      mapHeight = length warehouse
      mapWidth = length (head warehouse)
      answerP1 = foldl (\baseAcc y -> foldl (\acc x -> if canRemoveRollOfPaper warehouse (x, y) then acc + 1 else acc) baseAcc [0 .. mapWidth - 1]) 0 [0 .. mapHeight - 1]
   in (answerP1, solveP2 warehouse mapHeight mapWidth)

solveP2 :: [String] -> Int -> Int -> Int
solveP2 warehouse mapHeight mapWidth =
  let toRemove = foldl (\baseAcc y -> foldl (\acc x -> if canRemoveRollOfPaper warehouse (x, y) then (x, y) : acc else acc) baseAcc [0 .. mapWidth - 1]) [] [0 .. mapHeight - 1]
   in if length toRemove > 0 then length toRemove + solveP2 (removeFromMap warehouse toRemove) mapHeight mapWidth else 0

removeFromMap :: [String] -> [(Int, Int)] -> [String]
removeFromMap warehouse [] = warehouse
removeFromMap warehouse ((x, y):xs) =
  let row = warehouse !! y
      newRow = take x row ++ '.' : drop (x + 1) row
   in removeFromMap (take y warehouse ++ [newRow] ++ drop (y + 1) warehouse) xs

canRemoveRollOfPaper :: [String] -> (Int, Int) -> Bool
canRemoveRollOfPaper warehouse (x, y)
  | isRolOfPaper warehouse (x, y) =
      let neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]
          neighborValues = foldl (\acc (x', y') -> if isRolOfPaper warehouse (x', y') then acc + 1 else acc) 0 neighbors
       in neighborValues < 4
  | otherwise = False

isRolOfPaper :: [String] -> (Int, Int) -> Bool
isRolOfPaper warehouse (x, y) = atMapPosition warehouse (x, y) == '@'

atMapPosition :: [String] -> (Int, Int) -> Char
atMapPosition warehouse (x, y)
  | x >= 0 && x < length (head warehouse) && y >= 0 && y < length warehouse = warehouse !! y !! x
  | otherwise = '.'
