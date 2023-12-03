module DayTwo where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, liftM)
import Data.Char (isDigit, isSpace)
import Data.List (nub)

dayTwo :: IO ()
dayTwo = do
  inputLines <- lines <$> readFile "data/day_two.txt"
  case partA inputLines of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB inputLines of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left _ -> putStrLn "error running partB"

-- input parser
data Error i
  = EOF
  | Unexpected i
  | Empty
  deriving (Eq, Show)

newtype Parser i a = Parser
  {parse :: [i] -> Either [Error i] (a, [i])}

instance Monad (Parser i) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    parse (k output) rest

instance Functor (Parser i) where
  fmap = liftM

instance Applicative (Parser i) where
  pure a = Parser $ \input -> Right (a, input)
  (<*>) = ap

instance (Eq i) => Alternative (Parser i) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

satisfy :: (i -> Bool) -> Parser i i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EOF]
    hd : rst
      | predicate hd -> Right (hd, rst)
      | otherwise -> Left [Unexpected hd]

char :: (Eq i) => i -> Parser i i
char i = satisfy (== i)

string :: (Eq i) => [i] -> Parser i [i]
string = traverse char

digit :: Parser Char Char
digit = satisfy isDigit

whitespace :: Parser Char [Char]
whitespace = (:) <$> satisfy isSpace <*> whitespace <|> pure []

digits :: Parser Char [Char]
digits = (:) <$> digit <*> digits <|> pure []

integer :: Parser Char Int
integer = read <$> digits

eof :: Parser i ()
eof = Parser $ \input ->
  case input of
    [] -> Right ((), [])
    hd : _ -> Left [Unexpected hd]

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

type Round = [(Color, Int)]

data Game = Game
  { gameId :: Int,
    gameRounds :: [Round]
  }
  deriving (Show, Eq)

red :: Parser Char Color
red = string "red" >> pure Red

blue :: Parser Char Color
blue = string "blue" >> pure Blue

green :: Parser Char Color
green = string "green" >> pure Green

color :: Parser Char Color
color = red <|> blue <|> green

count :: Parser Char (Color, Int)
count = do
  _ <- whitespace
  int <- integer
  _ <- whitespace
  c <- color
  return (c, int)

round :: Parser Char Round
round = (:) <$> count <*> ((char ',' >> DayTwo.round) <|> (char ';' <|> (eof >> pure ' ') >> pure []))

rounds :: Parser Char [Round]
rounds = (:) <$> DayTwo.round <*> rounds <|> pure []

game :: Parser Char Game
game = do
  _ <- string "Game "
  int <- integer <* char ':'
  rs <- rounds
  return Game {gameId = int, gameRounds = rs}

-- Part A

possibleResult :: (Color, Int) -> Bool
possibleResult result = case result of
  (Red, x) -> x <= redInBag
  (Green, x) -> x <= greenInBag
  (Blue, x) -> x <= blueInBag
  where
    redInBag = 12
    greenInBag = 13
    blueInBag = 14

possibleGame :: Game -> Bool
possibleGame g = all (all possibleResult) (gameRounds g)

partA :: [String] -> Either [Error Char] Int
partA ls = do
  games <- mapM (parse game) ls
  Right (sum (map gameId (filter possibleGame (map fst games))))

-- Part B
maxColor :: Color -> Game -> Int
maxColor c g = maximum (map snd (filter (\draw -> c == fst draw) (concat (gameRounds g))))

powers :: (Int, Int, Int) -> Int
powers (x, y, z) = x * y * z

partB :: [String] -> Either [Error Char] Int
partB ls = do
  parserResult <- mapM (parse game) ls
  let games = map fst parserResult
  let maxRed = map (maxColor Red) games
  let maxGreen = map (maxColor Green) games
  let maxBlue = map (maxColor Blue) games
  Right (sum (map powers (zip3 maxRed maxGreen maxBlue)))
