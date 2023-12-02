module Main where

import DayOne (dayOne)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "one" : _ -> dayOne
    _ -> error "day not supplied"
