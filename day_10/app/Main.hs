module Main where

import System.CPUTime

main :: IO ()
main = do
  start <- getCPUTime
  puzzleContents <- readFile "puzzle.txt"
  print $ solve puzzleContents
  end <- getCPUTime
  let diff = (fromIntegral (end - start) / 1000000000 :: Double)
  putStrLn ("Execution time: " ++ show diff ++ " ms")

parse :: String -> [([Bool], [[Int]], [Int])]
parse puzzleText = map parseMachine $ map (\l -> split ' ' l) $ filter (/= "") $ lines puzzleText

parseMachine :: [String] -> ([Bool], [[Int]], [Int])
parseMachine (indicatorLightsSchematicsRaw : remainder) =
  let buttonWireSchematics = map parseCommaList $ take (length remainder - 1) remainder
      juoltageRequirements = parseCommaList $ last remainder
      indicatorLightsSchematics = map (\x -> x == '#') $ removeLastAndFirst indicatorLightsSchematicsRaw
   in (indicatorLightsSchematics, buttonWireSchematics, juoltageRequirements)

removeLastAndFirst :: [a] -> [a]
removeLastAndFirst xs = drop 1 $ take (length xs - 1) xs

parseCommaList :: String -> [Int]
parseCommaList xs = map read $ split ',' $ removeLastAndFirst xs

split :: Char -> String -> [String]
split _ [] = [""]
split delimiter (x:xs)
  | delimiter == x = "" : split delimiter xs
  | otherwise =
    let response = split delimiter xs
     in (x : response !! 0) : drop 1 response

solve :: String -> Int
solve input =
  let machines = parse input
   in foldl (\acc (desiredOutput, buttonWireSchematics, _) -> acc + (solveMachine desiredOutput buttonWireSchematics [take (length desiredOutput) $ repeat False] 1)) 0 $ machines

solveMachine :: [Bool] -> [[Int]] -> [[Bool]] -> Int -> Int
solveMachine _ _ _ 10 = 9999999
solveMachine desiredOutput buttonWireSchematics states attempt =
  let newStates = concat $ map (\buttonsToPress -> map (\state -> pressButtonSet buttonsToPress state) states) buttonWireSchematics
   in if any (== desiredOutput) newStates then 1 else 1 + (solveMachine desiredOutput buttonWireSchematics newStates (attempt + 1))

pressButtonSet :: [Int] -> [Bool] -> [Bool]
pressButtonSet [] state = state
pressButtonSet (buttonToPress : remainder) state =
  let newState = replace buttonToPress (not $ state !! buttonToPress) state
   in pressButtonSet remainder newState

replace :: Int -> a -> [a] -> [a]
replace index value xs = take index xs ++ [value] ++ drop (index + 1) xs
