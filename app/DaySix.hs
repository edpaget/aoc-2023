module DaySix where

import Parser

daySix :: IO ()
daySix = do
  input <- readFile "data/day_six.txt"
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left _ -> putStrLn "error running partB"


type Race = (Int, Int)


races :: Parser [Race]
races = do
  _ <- string "Time:"
  _ <- whitespace
  times <- integers
  _ <- char '\n'
  _ <- string "Distance:"
  _ <- whitespace
  distances <- integers
  return (zip times distances)


-- Part A
partA :: String -> Either [Error] Int
partA _ = do
  return 0


-- Part B
partB :: String -> Either [Error] Int
partB _ = do
  return 0
