module Main where

part1 :: String -> Int
part1 _ = 0

-- PART 2

part2 :: String -> Int
part2 _ = 0

main :: IO ()
main = do
  content <- readFile "test-input.txt"
  putStrLn $ "Part 1 answer " ++ show (part1 content)
  putStrLn $ "Part 2 answer " ++ show (part2 content)
