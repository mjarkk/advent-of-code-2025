module Main where

import Debug.Trace

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let (_, a, b) = solve puzzleContents
  print a
  print b

solve :: String -> (Int, Int, Int)
solve input = foldl moveDail (50, 0, 0) $ map parseLine $ filter (/= "") $ lines input

moveDail :: (Int, Int, Int) -> Int -> (Int, Int, Int)
moveDail (position, zeroCount, zeroClicksCount) v =
  let newPosition = abs ((position + v) `mod` 100)
      atZero = if newPosition == 0 then 1 else 0
      newZeroCount = zeroCount + atZero
      newZeroClicksCount = zeroClicksCount + atZero + movedOverZero position v
   in (newPosition, newZeroCount, newZeroClicksCount)

parseLine :: String -> Int
parseLine ('L' : x) = -(read x)
parseLine ('R' : x) = read x
parseLine _ = error "invalid input"

movedOverZero :: Int -> Int -> Int
movedOverZero position v
  | position == 0 = clicks
  | remainder < 0 = clicks + if position + remainder < 0 then 1 else 0
  | remainder > 0 = clicks + if position + remainder > 100 then 1 else 0
  where
    remainder = v `rem` 100
    clicks = abs (v `quot` 100)
