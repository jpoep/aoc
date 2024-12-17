{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack, unpack)
import Text.Regex.Base
import Text.Regex.TDFA

extractMults :: Text -> [Text]
extractMults input =
  let pattern :: Text = "mul\\([0-9]+,[0-9]+\\)"
   in getAllTextMatches (input =~ pattern) :: [Text]

extractNumbers :: Text -> (Int, Int)
extractNumbers input =
  let pattern :: Text = "[0-9]+"
      matches :: [Text] = getAllTextMatches (input =~ pattern)
   in case matches of
        (first : second : _) -> (read $ unpack first :: Int, read $ unpack second :: Int)
        _ -> (0, 0)

part1 :: String -> Int
part1 = sum . map (mult . extractNumbers) . extractMults . pack
    where
      mult (a, b) = a * b

-- PART 2

part2 :: String -> Int
part2 _ = 0

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ "content " ++ content
  putStrLn $ "patterns " ++ (show . extractMults . pack) content
  putStrLn $ "numbers " ++ (show . map extractNumbers . extractMults . pack) content
  putStrLn $ "Part 1 answer " ++ show (part1 content)
  putStrLn $ "Part 2 answer " ++ show (part2 content)
