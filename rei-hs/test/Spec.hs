--Integration Tests

import Test.HUnit
import Expr

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests =
  TestList
    [ TestLabel
        "test2"
        (TestCase (assertEqual "(1 :: Integer) should equal (1 :: Integer)" (1 :: Integer) (1 :: Integer)))
    ]
