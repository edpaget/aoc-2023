module DayEight where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace (trace)
import Parser

dayEight :: IO ()
dayEight = do
  input <- readFile "data/day_eight.txt"
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left err -> putStrLn ("error running partB" ++ show err)

type Choice = (String, String)

type TMap = Map String Choice

pair :: Parser (String, Choice)
pair = do
  node <- Parser.repeat (satisfy isAlpha)
  _ <- string " = ("
  left <- Parser.repeat (satisfy isAlpha)
  _ <- string ", "
  right <- Parser.repeat (satisfy isAlpha)
  _ <- string ")\n"
  return (node, (left, right))

tmap :: Parser (String, TMap)
tmap = do
  directions' <- Parser.repeat (char 'L' <|> char 'R')
  _ <- string "\n\n"
  pairs <- Parser.moreThanOnce $ Parser.repeat pair
  return (directions', Map.fromList pairs)

directions :: String -> String
directions str = str ++ directions str -- [c : s | s <- directions str, c <- str]

steps :: String -> String -> TMap -> String -> [String]
steps dirs' goal tmap' node
  | node == goal = []
  | otherwise = node : steps (tail dirs') goal tmap' nextNode
  where
    nextNodeChoice = case Map.lookup node tmap' of
      Just n -> n
      Nothing -> error "can't find next node in map"
    nextNode = case head dirs' of
      'L' -> fst nextNodeChoice
      'R' -> snd nextNodeChoice
      c -> error ("char is not allowed: " ++ show c)

-- Part A
partA :: String -> Either [Error] Int
partA input = do
  ((dirs, tmap'), _) <- parse tmap (input, 0, 0)
  return $ length $ steps (directions dirs) "ZZZ" tmap' "AAA"

-- Part B
stepsB :: String -> TMap -> String -> Int -> Int
stepsB dirs' tmap' node accum
  | "Z" `isSuffixOf` node = accum
  | otherwise = stepsB (tail dirs') tmap' nextNode $! (accum + 1)
  where
    nextNodeChoice = case Map.lookup node tmap' of
      Just n -> n
      Nothing -> error "can't find next node in map"
    nextNode = case head dirs' of
      'L' -> fst nextNodeChoice
      'R' -> snd nextNodeChoice
      c -> error ("char is not allowed: " ++ show c)

partB :: String -> Either [Error] Int
partB input = do
  ((dirs, tmap'), _) <- parse tmap (input, 0, 0)
  let allAs = filter (isSuffixOf "A") (Map.keys tmap')
  return $ foldl1 lcm $ map (\start -> stepsB (directions dirs) tmap' start 0) allAs
