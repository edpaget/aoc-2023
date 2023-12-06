module Parser where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, liftM)
import qualified Control.Monad.Fail as Fail
import Data.Char (isDigit, isSpace)
import Data.List (nub)

-- input parser
data Error
  = EOF
  | Unexpected Char
  | Empty
  deriving (Eq, Show)

type Input = ([Char], Int, Int)

type Result a = (a, Input)

row :: Input -> Int
row (_, x, _) = x

col :: Input  -> Int
col (_, _, x) = x

position :: Parser (Int, Int)
position = Parser $ \input ->
  Right ((row input, col input), input)

newtype Parser a = Parser
  {parse :: Input -> Either [Error] (Result a)}

instance Monad Parser where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, next) <- p input
    parse (k output) next

instance Fail.MonadFail Parser where
  fail _ = Parser $ \input ->
    case input of
      ([], _, _) -> Left [EOF]
      (hd:_, _, _) -> Left [Unexpected hd]

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  (<*>) = ap

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, next) -> Right (output, next)
      Right (output, next) -> Right (output, next)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    ([], _, _) -> Left [EOF]
    (hd : rst, rrow, ccol)
      | predicate hd -> case hd of
          '\n' -> Right (hd, (rst, rrow + 1, 0))
          _ -> Right (hd, (rst, rrow, ccol + 1))
      | otherwise -> Left [Unexpected hd]

char :: Char -> Parser Char
char i = satisfy (== i)

string :: [Char] -> Parser [Char]
string = traverse char

digit :: Parser Char
digit = satisfy isDigit

whitespace :: Parser [Char]
whitespace = (:) <$> satisfy isSpace <*> whitespace <|> pure []

digits :: Parser [Char]
digits = (:) <$> digit <*> digits <|> pure []

integer :: Parser Int
integer = read <$> digits

eof :: Parser ()
eof = Parser $ \input ->
  case input of
    ([], r, c) -> Right ((), ([], r, c))
    (hd : _, _, _) -> Left [Unexpected hd]
