module Main where

import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  puzzleContents <- readFile "puzzle.txt"
  let puzzleLines = lines puzzleContents
  print puzzleLines
  end <- getCPUTime
  let diff = round (fromIntegral (end - start) / 1000000000 :: Double)
  putStrLn ("Execution time: " ++ show diff ++ " ms")
