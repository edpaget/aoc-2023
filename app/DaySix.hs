module DaySix where

import Debug.Trace (trace)
import Parser

daySix :: IO ()
daySix = do
  input <- readFile "data/day_six.txt"
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left err -> putStrLn ("error running partB" ++ show err)

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

isWinner :: Race -> Int -> Bool
isWinner (time, distance) holdTime = (time - holdTime) * holdTime >= distance

winners :: Race -> Int
winners r@(time, _) = if even time then len * 2 - 1 else len * 2
  where
    len = length (filter (isWinner r) [0 .. time `div` 2])

-- Part A
partA :: String -> Either [Error] Int
partA input = do
  (rs, _) <- parse races (input, 0, 0)
  let winners' = trace ("rs: " ++ show rs) map winners rs
  return (product (trace ("ws: " ++ show winners') winners'))

-- Part B
intIgnoreWhitespace :: Parser Int
intIgnoreWhitespace = read <$> Parser.moreThanOnce (Parser.repeat (whitespace *> digit))

racesB :: Parser Race
racesB = do
  _ <- string "Time:"
  _ <- whitespace
  time <- intIgnoreWhitespace
  _ <- char '\n'
  _ <- string "Distance:"
  _ <- whitespace
  distance <- intIgnoreWhitespace
  return (time, distance)

partB :: String -> Either [Error] Int
partB input = do
  (rs, _) <- parse racesB (input, 0, 0)

  return (trace ("RS: " ++ show rs) (winners rs))
