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

solve :: String -> (Int, Int)
solve puzzle =
  let redTails = map (\(x, y) -> (read x :: Int, read y :: Int)) $ map (\l -> splitOnce ',' l) $ filter (/= "") $ lines puzzle
      allRedTailCombinations = allPairs redTails
      biggestArea =
        foldl
          ( \acc (a, b) ->
              let size = area a b
               in if size > acc then size else acc
          )
          0
          allRedTailCombinations
      (horizontalLines, verticalLines) = foldl lineReducer ([], []) $ zip redTails (drop 1 redTails ++ [redTails !! 0])
      biggestArea2 =
        foldl
          ( \acc ((a, b), idx) ->
              let size = area a b
               in if notOnSameLine a b && size > acc && validArea a b horizontalLines verticalLines redTails then size else acc
          )
          0
          $ zip allRedTailCombinations [0 ..]
   in (biggestArea, biggestArea2)

lineReducer :: ([(Int, Int, Int)], [(Int, Int, Int)]) -> ((Int, Int), (Int, Int)) -> ([(Int, Int, Int)], [(Int, Int, Int)])
lineReducer (horizontalLines, verticalLines) ((x1, y1), (x2, y2)) =
  let isVerticalLine = x1 == x2
   in if isVerticalLine
        then (horizontalLines, (x1, min y1 y2, max y1 y2) : verticalLines)
        else ((y1, min x1 x2, max x1 x2) : horizontalLines, verticalLines)

validArea :: (Int, Int) -> (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> Bool
validArea a b horizontalLines verticalLines redTails =
  let ((ax, ay), (bx, by)) = (a, b)
      (yMin, yMax) = (min ay by, max ay by)
      (xMin, xMax) = (min ax bx, max ax bx)
      fullArea = ((xMin, yMin), (xMax, yMax))
      validPoints = [a, b] --, (ax, by), (bx, ay)]

      pointsWithinAreaIncludingBorder = filter (\p -> p `notElem` validPoints && fst p >= xMin && fst p <= xMax && snd p >= yMin && snd p <= yMax) redTails
      pointsWithinArea = filter (\p -> withinArea fullArea p) pointsWithinAreaIncludingBorder
      allPointsWithinAreaHaveAdjacentPoint = null pointsWithinArea || all (\p -> any (\n -> p /= n && distance p n == 1) pointsWithinAreaIncludingBorder) pointsWithinArea

      horizontalLinesWithinArea =
        filter
          ( \(y, x1, x2) ->
              withinArea fullArea (x1, y) && not (withinArea fullArea (x2, y))
                || withinArea fullArea (x2, y) && not (withinArea fullArea (x1, y))
                || y > yMin && y < yMax && xMin >= x1 && xMax <= x2
          )
          horizontalLines
      zeroHorizontalLines = length horizontalLinesWithinArea == 0

      verticalLinesWithinArea =
        filter
          ( \(x, y1, y2) ->
              withinArea fullArea (x, y1) && not (withinArea fullArea (x, y2))
                || withinArea fullArea (x, y2) && not (withinArea fullArea (x, y1))
                || x > xMin && x < xMax && yMin >= y1 && yMax <= y2
          )
          verticalLines
      zeroVerticalLines = length verticalLinesWithinArea == 0
   in zeroHorizontalLines && zeroVerticalLines && allPointsWithinAreaHaveAdjacentPoint

withinArea :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
withinArea ((ax, ay), (bx, by)) (x, y) = x > ax && x < bx && y > ay && y < by

splitOnce :: Char -> String -> (String, String)
splitOnce n s = (takeWhile (/= n) s, drop 1 $ dropWhile (/= n) s)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = [(x, y) | y <- xs] ++ allPairs xs

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

notOnSameLine :: (Int, Int) -> (Int, Int) -> Bool
notOnSameLine (x1, y1) (x2, y2) = y1 /= y2 && x1 /= x2
