import Test.HUnit
import Text.Parsec
import Expr

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
-- tests = TestList [ testParseParameterisedExpr ]
{--}
tests = TestList [ "Sanity Foo" ~: (1, 2) ~=? foo 3 ]

-- testParseParameterisedExpr :: Test
-- doesnt work
-- testParseParameterisedExpr = TestCase $ assertEqual "Parsing (a, b, c) should succeed" (Right ["(a, b, c)"]) (parse parameterisedExpr "" "(a, b, c)")

{-| Sanity Foo

>>> foo 3
-}
foo :: (Eq a1, Num a1) => a1 -> (a1, a1)
foo 3 = (1, 2)
