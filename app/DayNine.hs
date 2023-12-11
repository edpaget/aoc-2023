module DayNine where

import Control.Applicative (Alternative (..))
import Debug.Trace (trace)
import Parser

dayNine :: IO ()
dayNine = do
  input <- readFile "data/day_nine.txt"
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left err -> putStrLn ("error running partB" ++ show err)

intLine :: Parser [Int]
intLine = moreThanOnce $ Parser.repeat (char ' ' *> integer <|> integer) <* (char '\n' <|> (eof >> pure ' '))

intLines :: Parser [[Int]]
intLines = Parser.repeat intLine

applyRule :: [Int] -> [[Int]]
applyRule ns
  | all (== 0) ns = []
  | otherwise = ns : applyRule (succ' ns)
  where
    succ' :: [Int] -> [Int]
    succ' [] = []
    succ' [_] = []
    succ' (hd:nxt:rst) = nxt - hd : succ' ( nxt : rst )

-- Part A
extrapolateA :: [[Int]] -> Int
extrapolateA lstNs = sum $ map last lstNs

partA :: String -> Either [Error] Int
partA input = do
  (ns, _) <- parse intLines (input, 0, 0)
  return $ sum (map (extrapolateA . applyRule) ns)

-- Part B
extrapolateB :: [[Int]] -> Int
extrapolateB = foldr ((-) . head) 0

partB :: String -> Either [Error] Int
partB input = do
  (ns, _) <- parse intLines (input, 0, 0)
  return $ sum (map (extrapolateB . applyRule) ns)
