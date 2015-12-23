module Main
where

import Test.Tasty
import Test.Tasty.HUnit
import InterpreterTest (main1, tests)
import ParserTest (main2, tests)

main = defaultMain $ testGroup "Tests"
    [ InterpreterTest.tests,
      ParserTest.tests
    ]