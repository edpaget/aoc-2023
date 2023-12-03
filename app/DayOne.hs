module DayOne where

import Data.Char (isDigit)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.Posix

dayOne :: IO ()
dayOne = do
  inputLines <- lines <$> readFile "data/day_one.txt"
  case applyDayOne firstDigit (firstDigit . reverse) inputLines of
    Nothing -> error "lines could not be read"
    Just x -> putStrLn ("Part A: " ++ (show x))
  case applyDayOne firstDigitOrNumberWord lastDigitOrNumberWord inputLines of
    Nothing -> error "lines could not be read"
    Just x -> putStrLn ("Part B: " ++ (show x))

-- Part A
firstDigit :: String -> Maybe Char
firstDigit "" = Nothing
firstDigit (c : _) | isDigit c = Just c
firstDigit (_ : str) = firstDigit str

-- Part B
digitRegex :: String
digitRegex = "one|two|three|four|five|six|seven|eight|nine"

firstDigitOrNumberWord :: String -> Maybe Char
firstDigitOrNumberWord "" = Nothing
firstDigitOrNumberWord str = do
  digits <- digitOrNumberWord str
  Just (head digits)

lastDigitOrNumberWord :: String -> Maybe Char
lastDigitOrNumberWord "" = Nothing
lastDigitOrNumberWord str = do
  digits <- digitOrNumberWord str
  Just (last digits)

digitOrNumberWord :: String -> Maybe [Char]
digitOrNumberWord str = mapM toChar (getAllTextMatches $ str =~ (printf "(%s|[0-9])" digitRegex :: String) :: [String])

toChar :: String -> Maybe Char
toChar "one" = Just '1'
toChar "two" = Just '2'
toChar "three" = Just '3'
toChar "four" = Just '4'
toChar "five" = Just '5'
toChar "six" = Just '6'
toChar "seven" = Just '7'
toChar "eight" = Just '8'
toChar "nine" = Just '9'
toChar (c : _) | isDigit c = Just c
toChar _ = Nothing

charsToInt :: Char -> Char -> Maybe Int
charsToInt a b = readMaybe (a : b : [])

applyDayOne :: (String -> Maybe Char) -> (String -> Maybe Char) -> [String] -> Maybe Int
applyDayOne getFirstDigit getLastDigit inputLines = do
  firstDigits <- mapM getFirstDigit inputLines
  lastDigits <- mapM getLastDigit inputLines
  coordinateInts <- mapM (uncurry $ charsToInt) (zip firstDigits lastDigits)
  Just (sum coordinateInts)
