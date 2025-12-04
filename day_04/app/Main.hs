module Main where

import Debug.Trace
import System.CPUTime

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
   in solveR warehouse mapHeight mapWidth

solveR :: [String] -> Int -> Int -> (Int, Int)
solveR warehouse mapHeight mapWidth =
  let toRemove = map (\(row, y) -> map (\(c, x) -> c == '@' && canRemoveRollOfPaper warehouse (x, y)) $ zip row [0 .. mapWidth - 1]) $ zip warehouse [0 .. mapHeight - 1]
      totalRemoved = foldl (\acc x -> if x then acc + 1 else acc) 0 $ concat toRemove
   in (totalRemoved, if totalRemoved > 0 then totalRemoved + snd (solveR (removeFromMap warehouse toRemove) mapHeight mapWidth) else 0)

removeFromMap :: [String] -> [[Bool]] -> [String]
removeFromMap warehouse toRemove = map (\(mapRow, removeRow) -> map (\(c, r) -> if r then '.' else c) $ zip mapRow removeRow) $ zip warehouse toRemove

canRemoveRollOfPaper :: [String] -> (Int, Int) -> Bool
canRemoveRollOfPaper warehouse (x, y) =
    let neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]
        neighborValues = foldl (\acc (x', y') -> if isRolOfPaper warehouse (x', y') then acc + 1 else acc) 0 neighbors
     in neighborValues < 4

isRolOfPaper :: [String] -> (Int, Int) -> Bool
isRolOfPaper warehouse (x, y) = atMapPosition warehouse (x, y) == '@'

atMapPosition :: [String] -> (Int, Int) -> Char
atMapPosition warehouse (x, y)
  | x >= 0 && x < length (head warehouse) && y >= 0 && y < length warehouse = warehouse !! y !! x
  | otherwise = '.'
