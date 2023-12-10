module DayFive where

import Control.Applicative (Alternative (..))
import Control.Monad (foldM)
import Data.Char (isAlpha)
import Data.Either (isRight)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Parser

dayFive :: IO ()
dayFive = do
  input <- readFile "data/day_five.txt"
  case partA input of
    Right x -> putStrLn ("PartA: " ++ show x)
    Left _ -> putStrLn "error running partA"
  case partB input of
    Right x -> putStrLn ("PartB: " ++ show x)
    Left _ -> putStrLn "error running partB"

data AlmanacRange = AlmanacRange
  { destination :: Int,
    source :: Int,
    range :: Int
  }
  deriving (Show, Eq)

data AlmanacMap = AlmanacMap
  { name :: String,
    ranges :: [AlmanacRange]
  }
  deriving (Show, Eq)

chars :: Parser [Char]
chars = Parser.repeat (satisfy isAlpha <|> char '-')

seeds :: Parser [Int]
seeds = string "seeds: " >> integers <* string "\n\n"

almanacRange :: Parser AlmanacRange
almanacRange = do
  d <- integer
  _ <- char ' '
  s <- integer
  _ <- char ' '
  r <- integer
  _ <- char '\n'
  return AlmanacRange {destination = d, source = s, range = r}

nonEmptyRange :: AlmanacRange -> Bool
nonEmptyRange AlmanacRange {range = r} = r > 0

fillRanges :: AlmanacMap -> AlmanacMap
fillRanges AlmanacMap {name = n, ranges = rs} = AlmanacMap {name = n, ranges = rs'}
  where
    rs' = filter nonEmptyRange $ fill 0 $ sortOn source rs
    fill :: Int -> [AlmanacRange] -> [AlmanacRange]
    fill idx [] = [AlmanacRange {destination = idx, source = idx, range = maxBound - idx}]
    fill idx (rng : rngs) = preFill : rng : fill nextIdx rngs
      where
        preFill = AlmanacRange idx idx (source rng - idx)
        nextIdx = source rng + range rng

almanacMap :: Parser AlmanacMap
almanacMap = do
  n <- chars
  _ <- string " map:\n"
  rs <- Parser.repeat almanacRange
  _ <- char '\n' <|> (eof >> pure ' ')
  return (fillRanges AlmanacMap {name = n, ranges = rs})

almanac :: Parser ([Int], [AlmanacMap])
almanac = do
  ss <- seeds
  amap <- Parser.repeat almanacMap
  return (ss, amap)

-- Part A
applyAlmanacRange :: Int -> AlmanacRange -> Either [Error] Int
applyAlmanacRange seed AlmanacRange {destination = d, source = s, range = r}
  | s <= seed && seed <= (s + r) = Right (seed + offset)
  | otherwise = Left [Empty]
  where
    offset = d - s

applyAlmanacMap :: Int -> AlmanacMap -> Either [Error] Int
applyAlmanacMap seed AlmanacMap {ranges = rs} =
  case filter isRight (map (applyAlmanacRange seed) rs) of
    [] -> Right seed
    hd : _ -> hd

applyAlmanac :: Int -> [AlmanacMap] -> Either [Error] Int
applyAlmanac = foldM applyAlmanacMap

partA :: String -> Either [Error] Int
partA input = do
  ((seeds', amap), _) <- parse almanac (input, 0, 0)
  seedLocations <- mapM (`applyAlmanac` amap) seeds'
  return (minimum seedLocations)

-- Part B
seedRanges :: [Int] -> [AlmanacRange]
seedRanges [] = []
seedRanges [_] = error "seeds must be event"
seedRanges (x : y : t) = AlmanacRange {destination = x, source = x, range = y} : seedRanges t

applyAlmanacRangeRng :: AlmanacRange -> AlmanacRange -> Maybe AlmanacRange
applyAlmanacRangeRng AlmanacRange {destination = destA, source = srcA, range = rngA} AlmanacRange {destination = destB, source = srcB, range = rngB}
  | nonEmptyRange rng = Just rng
  | otherwise = Nothing
  where
    dest = destB + max 0 (destA - srcB)
    src = srcA + max 0 (srcB - destA)
    len = min (destA + rngA) (srcB + rngB) - max destA srcB
    rng = AlmanacRange {destination = dest, source = src, range = len}

applyAlmanacMapRng :: AlmanacRange -> AlmanacMap -> [AlmanacRange]
applyAlmanacMapRng rng AlmanacMap {ranges = rs} = mapMaybe (applyAlmanacRangeRng rng) rs

applyAlmanacRng :: [AlmanacRange] -> [AlmanacMap] -> [AlmanacRange]
applyAlmanacRng = foldl (\r m -> concatMap (`applyAlmanacMapRng` m) r)

partB :: String -> Either [Error] Int
partB input = do
  ((seeds', amap), _) <- parse almanac (input, 0, 0)
  let seeds'' = seedRanges seeds'
  let seedLocations = map destination (applyAlmanacRng seeds'' amap)
  return (minimum seedLocations)
