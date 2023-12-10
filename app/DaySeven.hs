module DaySeven where

import Debug.Trace (trace)
import Data.List (sort, sortBy, group)
import Parser

daySeven :: IO ()
daySeven = do
  input <- readFile "data/day_seven.txt"
  print input
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left err -> putStrLn ("error running partB" ++ show err)

data Rank = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Show)

data HandScore = HighCard
  | Pair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind
  deriving (Eq, Ord, Enum, Show)

type Hand = ([Rank], Int)

-- compHand :: Hand -> Hand -> Ordering
-- compHand ([], _) ([], _) = EQ
-- compHand ([], _) (_, _) = error "inequal hands"
-- compHand (_, _) ([], _) = error "inequal hands"
-- compHand (hdA:rstA, bidA) (hdB:rstB, bidB)
--   | hdA > hdB = GT
--   | hdA > hdB = GT
--   | hdA == hdB = compare (rstA, bidA) (rstB, bidB)

-- instance Ord Hand where
--   compare = compHand

rankFromChar :: Char -> Either [Error] Rank
rankFromChar '2' = Right Two
rankFromChar '3' = Right Three
rankFromChar '4' = Right Four
rankFromChar '5' = Right Five
rankFromChar '6' = Right Six
rankFromChar '7' = Right Seven
rankFromChar '8' = Right Eight
rankFromChar '9' = Right Nine
rankFromChar 'T' = Right Ten
rankFromChar 'J' = Right Jack
rankFromChar 'Q' = Right Queen
rankFromChar 'K' = Right King
rankFromChar 'A' = Right Ace
rankFromChar c = Left [Unexpected c]

rank :: Parser Rank
rank = Parser $ \input ->
  case input of
    ([], _, _) -> Left [EOF]
    (hd:rst, r, c) -> case rankFromChar hd of
      Right rnk -> Right (rnk, (rst, r, c + 1))
      Left err -> Left err

hand :: Parser Hand
hand = (,) <$> (Parser.repeat rank <* whitespace) <*> (integer <* char '\n')

hands :: Parser [Hand]
hands = Parser.repeat hand

scoreHand :: [Rank] -> HandScore
scoreHand hnd = case sort (map length (group (sort hnd))) of
  [5] -> FiveKind
  [1, 4] -> FourKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeKind
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> Pair
  _ -> HighCard

compareHands :: Hand -> Hand -> Ordering
compareHands (hdA, _) (hdB, _) = handRnk hdA hdB
  where
    handRnk :: [Rank] -> [Rank] -> Ordering
    handRnk [] [] = EQ
    handRnk [] _ = error "unevenHands"
    handRnk _ [] = error "unevenHands"
    handRnk a b = compare (scoreHand a) (scoreHand b)

partA :: String -> Either [Error] Int
partA input = do
  (hnds, _) <- parse hands (input, 0, 0)
  let sHnds = sortBy (compareHands <> compare) hnds
  return (sum (zipWith (*) (map snd sHnds) [1..length sHnds+1]))

-- Part B
rankFromCharB :: Char -> Either [Error] Rank
rankFromCharB '2' = Right Two
rankFromCharB '3' = Right Three
rankFromCharB '4' = Right Four
rankFromCharB '5' = Right Five
rankFromCharB '6' = Right Six
rankFromCharB '7' = Right Seven
rankFromCharB '8' = Right Eight
rankFromCharB '9' = Right Nine
rankFromCharB 'T' = Right Ten
rankFromCharB 'J' = Right Joker
rankFromCharB 'Q' = Right Queen
rankFromCharB 'K' = Right King
rankFromCharB 'A' = Right Ace
rankFromCharB c = Left [Unexpected c]

rankB :: Parser Rank
rankB = Parser $ \input ->
  case input of
    ([], _, _) -> Left [EOF]
    (hd:rst, r, c) -> case rankFromCharB hd of
      Right rnk -> Right (rnk, (rst, r, c + 1))
      Left err -> Left err

handB :: Parser Hand
handB = (,) <$> (Parser.repeat rankB <* whitespace) <*> (integer <* char '\n')

handsB :: Parser [Hand]
handsB = Parser.repeat handB

scoreHandB :: [Rank] -> HandScore
scoreHandB hnd = case (jokerCount, sort (map length (group (sort hnd)))) of
  (_, [5]) -> FiveKind
  (4, [1, 4]) -> FiveKind
  (1, [1, 4]) -> FiveKind
  (_, [1, 4]) -> FourKind
  (3, [2, 3]) -> FiveKind
  (2, [2, 3]) -> FiveKind
  (1, [2, 3]) -> FourKind
  (_, [2, 3]) -> FullHouse
  (3, [1, 1, 3]) -> FourKind
  (1, [1, 1, 3]) -> FourKind
  (_, [1, 1, 3]) -> ThreeKind
  (2, [1, 2, 2]) -> FourKind
  (1, [1, 2, 2]) -> FullHouse
  (_, [1, 2, 2]) -> TwoPair
  (2, [1, 1, 1, 2]) -> ThreeKind
  (1, [1, 1, 1, 2]) -> ThreeKind
  (_, [1, 1, 1, 2]) -> Pair
  (1, _) -> Pair
  _ -> HighCard
  where jokerCount = length (filter (== Joker) hnd)

compareHandsB :: Hand -> Hand -> Ordering
compareHandsB (hdA, _) (hdB, _) = handRnk hdA hdB
  where
    handRnk :: [Rank] -> [Rank] -> Ordering
    handRnk [] [] = EQ
    handRnk [] _ = error "unevenHands"
    handRnk _ [] = error "unevenHands"
    handRnk a b = compare (scoreHandB a) (scoreHandB b)

partB :: String -> Either [Error] Int
partB input = do
  (hnds, _) <- parse handsB (input, 0, 0)
  let sHnds = sortBy (compareHandsB <> compare) hnds
  return (trace ("hnds" ++ show (zip (map fst sHnds) (map (scoreHandB . fst) sHnds))) (sum (zipWith (*) (map snd sHnds) [1..length sHnds+1])))
