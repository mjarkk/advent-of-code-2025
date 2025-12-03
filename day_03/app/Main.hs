module Main where

main :: IO ()
main = print . solve =<< readFile "puzzle.txt"

parse :: String -> [[Int]]
parse input = map (map (\c -> read [c])) $ filter (/= "") $ lines input

solve :: String -> (Int, Int)
solve puzzleInput = foldl (\(p1, p2) row -> (p1 + solveRow 2 row, p2 + solveRow 12 row)) (0, 0) $ parse puzzleInput

solveRow :: Int -> [Int] -> Int
solveRow n row = read $ concatMap show $ fst $ foldl (\acc idx -> takeHighestNum acc idx n row) ([], 0) [0 .. n - 1]

takeHighestNum :: ([Int], Int) -> Int -> Int -> [Int] -> ([Int], Int)
takeHighestNum (accList, accOffset) idx n row =
  let searchSpace = drop accOffset $ take (length row - (n - idx - 1)) row
      (val, valIdx) = foldl (\(maxVal, maxIdx) (fv, fi) -> if fv > maxVal then (fv, fi) else (maxVal, maxIdx)) (-1, -1) $ zip searchSpace [0 ..]
   in (accList ++ [val], accOffset + valIdx + 1)
