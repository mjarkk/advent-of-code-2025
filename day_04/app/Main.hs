module Main where

import Data.Array.Unboxed
import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  puzzleContents <- readFile "puzzle.txt"
  print $ solve puzzleContents
  end <- getCPUTime
  let diff = (fromIntegral (end - start) / 1000000000 :: Double)
  putStrLn ("Execution time: " ++ show diff ++ " ms")

solve :: String -> (Int, Int)
solve input =
  let warehouse = filter (/= "") $ lines input
      mapHeight = length warehouse
      mapWidth = length $ warehouse !! 0
      warehouseArray = listArray ((0, 0), (mapHeight - 1, mapWidth - 1)) [c | row <- warehouse, c <- row] :: UArray (Int, Int) Char
   in solveR warehouseArray mapHeight mapWidth

solveR :: UArray (Int, Int) Char -> Int -> Int -> (Int, Int)
solveR warehouse mapHeight mapWidth =
  let toRemove = [(y, x) | y <- [0 .. mapHeight - 1], x <- [0 .. mapWidth - 1], warehouse ! (y, x) == '@' && canRemoveRollOfPaper warehouse (x, y)]
      removed = length toRemove
   in if removed > 0
        then
          let newWarehouse = warehouse // [((y, x), '.') | (y, x) <- toRemove]
           in (removed, removed + snd (solveR newWarehouse mapHeight mapWidth))
        else (0, 0)

canRemoveRollOfPaper :: UArray (Int, Int) Char -> (Int, Int) -> Bool
canRemoveRollOfPaper warehouse (x, y) =
  let neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]
      neighborValues = foldl (\acc (x', y') -> acc + isRolOfPaper warehouse (x', y')) 0 neighbors
   in neighborValues < 4

isRolOfPaper :: UArray (Int, Int) Char -> (Int, Int) -> Int
isRolOfPaper warehouse (x, y)
  | x >= minX && x <= maxX && y >= minY && y <= maxY && warehouse ! (y, x) == '@' = 1
  | otherwise = 0
  where ((minX, minY), (maxX, maxY)) = bounds warehouse
