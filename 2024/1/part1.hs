module Main where

import Data.List (sort)
import Data.List.Split
import Text.Parse

convertToList :: String -> [String]
convertToList = splitOn "\n"

splitEntries :: [String] -> [(Int, Int)]
splitEntries list = [(read (head (words x)) :: Int, read (last (words x)) :: Int) | x <- list, not $ null x]


separateLists :: [(Int, Int)] -> ([Int], [Int])
separateLists list = ([fst x | x <- list], [snd x | x <- list])

parseInput :: String -> ([Int], [Int])
parseInput = separateLists . splitEntries . convertToList

sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (list1, list2) = (sort list1, sort list2)

getDifferences :: ([Int], [Int]) -> Int
getDifferences input = sum [abs (x - y) | (x, y) <- uncurry zip input]

getOutput = getDifferences . sortLists . parseInput

main :: IO ()
main = do
  content <- readFile "input.txt"
  let output = getOutput content
  putStrLn $ "The difference is: " ++ show output
