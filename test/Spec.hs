import Lib
import Test.HUnit

firsttest = TestCase 
  (assertEqual "are equal"
    1 1)

secondtest = TestCase 
  (assertEqual "are not equal"
    1 2)

tests = test
  [ TestLabel "first test" firsttest
  , TestLabel "second test" secondtest
  ]

main :: IO Counts
main = runTestTT tests
