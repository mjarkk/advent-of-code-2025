module Main where

main :: IO ()
main = do
    puzzleContents <- readFile "puzzle.txt"
    let puzzleLines = lines puzzleContents
    print puzzleLines
