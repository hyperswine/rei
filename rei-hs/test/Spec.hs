--Integration Tests

import Test.HUnit
import Text.Parsec
import Expr

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [ testParseParameterisedExpr ]

testParseParameterisedExpr :: Test
testParseParameterisedExpr = TestCase $ assertEqual "Parsing (a, b, c) should succeed" (Right ["(a, b, c)"]) (parse parameterisedExpr "" "(a, b, c)")
