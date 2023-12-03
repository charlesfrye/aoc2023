module Main (main) where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = parseInput contents
      calVals = map (extractCalVal . tokenize) input
      calVals' = map (extractCalVal . tokenize') input
      total = sumCalVals calVals
      total' = sumCalVals calVals'
  print total
  print total'
