module DayTwo where

import Control.Applicative (Alternative (..))
import Parser

dayTwo :: IO ()
dayTwo = do
  inputLines <- lines <$> readFile "data/day_two.txt"
  case partA inputLines of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB inputLines of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left _ -> putStrLn "error running partB"

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

type Round = [(Color, Int)]

data Game = Game
  { gameId :: Int,
    gameRounds :: [Round]
  }
  deriving (Show, Eq)

red :: Parser Color
red = string "red" >> pure Red

blue :: Parser Color
blue = string "blue" >> pure Blue

green :: Parser Color
green = string "green" >> pure Green

color :: Parser Color
color = red <|> blue <|> green

count :: Parser (Color, Int)
count = do
  _ <- whitespace
  int <- integer
  _ <- whitespace
  c <- color
  return (c, int)

round :: Parser Round
round = (:) <$> count <*> ((char ',' >> DayTwo.round) <|> (char ';' <|> (eof >> pure ' ') >> pure []))

rounds :: Parser [Round]
rounds = (:) <$> DayTwo.round <*> rounds <|> pure []

game :: Parser Game
game = do
  _ <- string "Game "
  int <- integer <* char ':'
  rs <- rounds
  return Game {gameId = int, gameRounds = rs}


linesToInput :: [String] -> [Input]
linesToInput ls = zip3 ls [0..length ls] (repeat 0)

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

partA :: [String] -> Either [Error] Int
partA ls = do
  games <- mapM (parse game) (linesToInput ls)
  Right (sum (map gameId (filter possibleGame (map fst games))))

-- Part B
maxColor :: Color -> Game -> Int
maxColor c g = maximum (map snd (filter (\draw -> c == fst draw) (concat (gameRounds g))))

powers :: (Int, Int, Int) -> Int
powers (x, y, z) = x * y * z

partB :: [String] -> Either [Error] Int
partB ls = do
  parserResult <- mapM (parse game) (linesToInput ls)
  let games = map fst parserResult
  let maxRed = map (maxColor Red) games
  let maxGreen = map (maxColor Green) games
  let maxBlue = map (maxColor Blue) games
  Right (sum (map powers (zip3 maxRed maxGreen maxBlue)))
