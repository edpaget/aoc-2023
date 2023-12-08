module DayFour where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.List (intersect)
import Parser

dayFour :: IO ()
dayFour = do
  input <- readFile "data/day_four.txt"
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left _ -> putStrLn "error running partB"

data Card = Card Int [Int] [Int] deriving (Show, Eq)

cardId :: Parser Int
cardId = string "Card" *> whitespace *> integer <* char ':'

integers :: Parser [Int]
integers = Parser.repeat (whitespace *> integer)

card :: Parser Card
card = do
  cid <- cardId
  owned <- integers
  _ <- whitespace
  _ <- char '|'
  winning <- integers
  _ <- char '\n'
  return (Card cid owned winning)

cards :: Parser [Card]
cards = Parser.repeat card

-- Part A
scoreCard :: Card -> Int
scoreCard (Card _ owned winning) = if scorePower >= 0 then 2 ^ scorePower else 0
  where
    scorePower = length (owned `intersect` winning) - 1

partA :: String -> Either [Error] Int
partA input = do
  (cs, _) <- parse cards (input, 0, 0)
  return (sum (map scoreCard cs))

-- Part B
getCardId :: Card -> Int
getCardId (Card c _ _) = c

matches :: Card -> Int
matches (Card _ owned winning) = length (owned `intersect` winning)

copyRange :: Card -> [Int]
copyRange c@(Card idx _ _ ) = if matchCount > 0 then [idx + 1..idx + matchCount] else []
  where matchCount = matches c

cardCopyCount :: Card -> Map.Map Int Int -> Int
cardCopyCount (Card cid _ _) m = Maybe.fromMaybe 0 (Map.lookup cid m)

updateCopyCount :: Map.Map Int Int -> (Card, [Int]) -> Map.Map Int Int
updateCopyCount mmap (c, xs) = foldl (\m x -> Map.insertWith (+) x (cardCopyCount c m) m) mmap xs

sumCopies :: Map.Map Int Int -> Int -> Int -> Int
sumCopies copies idx n = case Map.lookup idx copies of
  Just v -> v + n
  Nothing -> n

partB :: String -> Either [Error] Int
partB input = do
  (cs, _) <- parse cards (input, 0, 0)
  let ranges = zip cs (map copyRange cs)
  let cids = map getCardId cs
  let maxId = maximum cids
  let copies = foldl updateCopyCount (Map.fromList (map (, 1) cids)) ranges
  return (foldr (sumCopies copies) 0 [0..maxId])
