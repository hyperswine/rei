module Main (main) where

import Expr
import Text.Parsec
import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result
       
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> return []                -- No comma?  Return [], no more cells

cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")
       
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

l :: Parser Char
l = char 'a'

parseString :: String -> Either ParseError String
parseString = parse parenParamList "(a, b, c, d)"

--------------------------------
--  MAIN
--------------------------------

main :: IO ()
main = print $ parseString "a"
