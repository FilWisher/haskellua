import Language.Lua.DSL
import Test.HUnit

firsttest = 1 ~?= 1

secondtest = 1 ~?= 2

tests = test
  [ TestLabel "first test" firsttest
  , TestLabel "second test" secondtest
  ]

main :: IO Counts
main = runTestTT tests
