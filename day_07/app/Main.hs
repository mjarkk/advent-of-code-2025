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

solve :: String -> (Int, Int)
solve puzzle =
  let input = filter (/= "") $ lines puzzle
      start = indexOf (input !! 0) 'S'
   in (solveR input [start], sum $ map snd $ solveRP2 input [(start, 1)])

indexOf :: Eq a => [a] -> a -> Int
indexOf [] _ = -1
indexOf (x : xs) c
    | x == c = 0
    | otherwise = 1 + indexOf xs c

indexOfFst :: Eq a => [(a, b)] -> a -> Int
indexOfFst [] _ = -1
indexOfFst ((x, _) : xs) c
    | x == c = 0
    | otherwise =
      let result = indexOfFst xs c
       in if result == -1 then -1 else 1 + result

solveR :: [String] -> [Int] -> Int
solveR [] _ = 0
solveR (row : remainder) beams =
  let (newBeams, totalSplits) = foldl (\(newSplits, splits) (c, idx) -> case (c == '^', elem idx beams) of
        (True, True) -> (idx - 1 : idx + 1 : newSplits, splits + 1)
        (False, True) -> (idx : newSplits, splits)
        _ -> (newSplits, splits)) ([], 0) $ zip row [0..]
   in totalSplits + solveR remainder newBeams

solveRP2 :: [String] -> [(Int, Int)] -> [(Int, Int)]
solveRP2 [] acc = acc
solveRP2 (row : remainder) beams =
    let foldfn acc (c, idx)
          | c == '^' = addBeam (idx + 1, beamValue) $ addBeam (idx - 1, beamValue) acc
          | beamIdx /= -1 = addBeam (idx, beamValue) acc
          | otherwise = acc
          where
            beamIdx = indexOfFst beams idx
            beamValue = if beamIdx == -1 then 0 else snd (beams !! beamIdx)
        newBeams = foldl foldfn [] $ zip row [0..]
    in solveRP2 remainder $ if length newBeams == 0 then beams else newBeams

addBeam :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addBeam (idx, value) beams
    | foundIdx == -1 = (idx, value) : beams
    | otherwise =
        let prevBeam = beams !! foundIdx
         in take foundIdx beams ++ [(idx, value + snd prevBeam)] ++ drop (foundIdx + 1) beams
    where
        foundIdx = indexOfFst beams idx
