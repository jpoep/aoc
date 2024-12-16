module Main where

import Data.List.Split

convertToList :: String -> [String]
convertToList = splitOn "\n"

convertLine :: String -> [Int]
convertLine = map read . splitOn " "

toMatrix :: String -> [[Int]]
toMatrix = map convertLine . convertToList

tupleWindows :: [a] -> [(a, a)]
tupleWindows [] = []
tupleWindows list@(x : xs) = zip list xs -- by zipping the list with its tail, we get a list of tuples with the current element and the next element

allIncreasesWithinThreshold :: [Int] -> Bool
allIncreasesWithinThreshold = all increaseWithinThreshold . tupleWindows
  where
    difference (x, y) = abs (x - y)
    increaseWithinThreshold tuple = difference tuple > 0 && difference tuple < 4

isContinuous :: [Int] -> Bool
isContinuous = allContinuous . map difference . tupleWindows
  where
    difference (x, y) = x - y
    allContinuous a = all (> 0) a || all (< 0) a

isLineSafe :: [Int] -> Bool
-- My original version below:
-- isLineSafe line = allIncreasesWithinThreshold line && isContinuous line
-- After looking up whether this can be eta reduced without fully understanding it:
isLineSafe = (&&) <$> allIncreasesWithinThreshold <*> isContinuous

part1 :: String -> Int
part1 = length . filter isLineSafe . toMatrix

-- PART 2

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where
    (lft, _ : rgt) = splitAt idx xs

applyProblemDampener :: [a] -> [[a]]
applyProblemDampener xs = map (`deleteAt` xs) [0 .. length xs - 1]

isSafeAfterDampening :: [Int] -> Bool
isSafeAfterDampening = any isLineSafe . applyProblemDampener

part2 :: String -> Int
part2 = length . filter isSafeAfterDampening . toMatrix

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ "Part 1 answer " ++ show (part1 content)
  putStrLn $ "Part 2 answer " ++ show (part2 content)
