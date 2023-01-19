module Main where

import Data.String
-- error in ghci
import Expr
import Text.Parsec

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

stringParser :: Parsec String st String
stringParser = many anyChar
