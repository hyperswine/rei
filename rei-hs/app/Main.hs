module Main (main) where

import Expr
import Text.Parsec

l = char 'a'

main :: IO ()
main = show $ parse l "a"

-- b = parameterisedExpr "(a, b, c)"

b :: Module
b = "Hi"
