module Main where

import Debug.Trace

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  print $ solve puzzleContents

parse :: String -> [(Int, Int)]
parse input = map parseSection $ split (removeChar input '\n') ','

parseSection :: String -> (Int, Int)
parseSection section =
  let [left, right] = split section '-'
   in (read left, read right)

split :: String -> Char -> [String]
split "" _ = [""]
split (x : xs) needle
  | x == needle = "" : sets
  | otherwise = (x : head sets) : tail sets
  where
    sets = split xs needle

removeChar :: String -> Char -> String
removeChar "" _ = ""
removeChar (x : xs) needle
  | x == needle = removeChar xs needle
  | otherwise = x : removeChar xs needle

solve :: String -> (Int, Int)
solve puzzleContents = foldl solveSection (0, 0) $ parse puzzleContents

solveSection :: (Int, Int) -> (Int, Int) -> (Int, Int)
solveSection acc (from, to) = foldl countRepeatingRanges acc [from .. to]

countRepeatingRanges :: (Int, Int) -> Int -> (Int, Int)
countRepeatingRanges (a,b) num = (countRepeatingRangesPart1 a num, countRepeatingRangesPart2 b num)

countRepeatingRangesPart1 :: Int -> Int -> Int
countRepeatingRangesPart1 acc num
  | length str `mod` 2 == 1 = acc
  | otherwise =
        let rangeToCheck = length str `div` 2
            rangeAndRemainder = splitAt rangeToCheck str
            repeats = uncurry rangeRepeats rangeAndRemainder
         in acc + if repeats then read str else 0
  where
    str = show num

countRepeatingRangesPart2 :: Int -> Int -> Int
countRepeatingRangesPart2 acc num =
  let str = show num
      l = length str
      rangesToCheck = filter (\range -> l `mod` range == 0) [1..length str `div` 2]
      repeats = any (\range -> uncurry rangeRepeats $ splitAt range str) rangesToCheck
   in acc + if repeats then read str else 0

rangeRepeats :: String -> String -> Bool
rangeRepeats _ "" = True
rangeRepeats pat s
  | take l s == pat = rangeRepeats pat $ drop l s
  | otherwise = False
  where
    l = length pat
