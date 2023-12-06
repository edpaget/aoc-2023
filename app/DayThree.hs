module DayThree where

import Parser
import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isNumber)
import Data.Maybe (catMaybes)
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map

dayThree :: IO ()
dayThree = do
  input <- readFile "data/day_three.txt"
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left _ -> putStrLn "error running partB"

data PartInformation = Symbol Char (Int, Int)
  | PartNumber [Char] (Int, Int)
  deriving (Show, Eq)

period :: Parser Char
period = char '.' <|> char '\n'

periodspace :: Parser [Char]
periodspace = (:) <$> period <*> periodspace <|> pure []

symbol :: Parser PartInformation
symbol = do
  pos <- position
  sym <- satisfy (\c -> not (isAlpha c || isNumber c || c == '.'))
  return (Symbol sym pos)

partNumber :: Parser PartInformation
partNumber = do
  startPosition <- position
  (n:ns) <- digits
  return (PartNumber (n:ns) startPosition)

part :: Parser PartInformation
part = symbol <|> partNumber

parts :: Parser [PartInformation]
parts = (:) <$> (periodspace *> part) <*> parts <|> pure []

-- Part A
adjacentToSymbol :: Set.Set (Int, Int) -> [Char] -> (Int, Int) -> Bool
adjacentToSymbol set ds (rrow, ccol) = not (Set.null (set `Set.intersection` adjacentPositions))
  where lengthDs = length ds
        adjacentPositions = Set.fromList [(x, y) | x <- [rrow-1..rrow+1], y <- [ccol-1..ccol+lengthDs]]

partA :: String -> Either [Error] Int
partA input = do
  (partInfo, _) <- parse parts (input, 0, 0)
  let symbols = Set.fromList [p| (Symbol _ p) <- partInfo]
  let partNos = [num | (PartNumber num pos) <- partInfo, adjacentToSymbol symbols num pos]
  Right (sum (map read partNos))

-- Part B
partPositions :: (String, (Int, Int)) -> [((Int, Int), (Int, Int))]
partPositions (ds, (rrow, ccol)) = [((rrow,y), (rrow, ccol)) | y <- [ccol..ccol+lengthDs-1]]
  where lengthDs = length ds

possibleGears :: [(String, (Int, Int))] -> Map.Map (Int, Int) (Int,Int)
possibleGears partNos = Map.fromList (concatMap partPositions partNos)

gearParts :: Map.Map (Int, Int) (Int, Int) -> Map.Map (Int, Int) Int -> (Int, Int) -> [Int]
gearParts  partPosMap partMap (grow, gcol) = catMaybes [Map.lookup gearPosition partMap | gearPosition <- gearPositions]
  where adjacentToGear = [(x,y) | x <- [grow-1..grow+1], y <- [gcol-1..gcol+1]]
        gearPositions = nub (catMaybes [Map.lookup adjacentPos partPosMap | adjacentPos <- adjacentToGear])

gearRatio :: [Int] -> Int
gearRatio [x, y] = x * y
gearRatio _ = error "not exactly two gears"

partB :: String -> Either [Error] Int
partB input = do
  (partInfo, _) <- parse parts (input, 0, 0)
  let gears = [p| (Symbol '*' p) <- partInfo]
  let partMap = possibleGears [(num, pos) | (PartNumber num pos) <- partInfo]
  let partNos = Map.fromList [(pos, read num) | (PartNumber num pos) <- partInfo]
  Right (sum (map gearRatio (filter (\matches -> length matches == 2) (map (gearParts partMap partNos) gears))))
