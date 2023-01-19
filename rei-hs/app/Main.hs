module Main where

import Data.String
import Expr
import Text.Parsec

main :: IO ()
main = putStrLn "Hello, Rei!"

stringParser :: Parsec String st String
stringParser = many anyChar
