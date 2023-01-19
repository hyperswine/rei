module Main where

import Data.String
import Expr
import Text.Parsec
-- import Test.HUnit

main :: IO ()
main = putStrLn "Hello, Rei!"

stringParser :: Parsec String st String
stringParser = many anyChar
