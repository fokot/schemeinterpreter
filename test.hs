module Schemeinterpreter.Test
where

import Test.Tasty
import Test.Tasty.HUnit
import Schemeinterpreter hiding (main)
import Text.ParserCombinators.Parsec (parse)
import Text.Parsec.Prim
import Data.Functor.Identity

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parseNumerUnitTests]

fromRight (Right x) = x
fromRight (Left _) = error "Can not parse it!!! This is not right!!!"

isRight (Right _) = True
isRight (Left _) = False

parses :: Parsec String () a -> String -> a
parser `parses` toParse = fromRight $ parse parser "test" toParse

notParses :: Parsec String () a -> String -> Assertion
parser `notParses` toParse = if isRight (parse parser "test" toParse) then
                                assertString $ show toParse ++ " should not be parsable"
                             else return ()

parseNumerUnitTests = testGroup "parseNumber Unit tests"
  [ testCase "parses decimals by default" $
    parseNumber `parses` "111" @?= Number 111
    ,
    testCase "parses binary" $
    parseNumber `parses` "#b111" @?= Number 7
    ,
    testCase "parses octal" $
    parseNumber `parses` "#o111" @?= Number (8 * 8 + 8 + 1)
    ,
    testCase "parses decimal" $
    parseNumber `parses` "#d111" @?= Number 111
    ,
    testCase "parses hexadecimal" $
    parseNumber `parses` "#x1a1" @?= Number 417
    ,
    testCase "does not parse bullshit" $
    parseNumber `notParses` "#111"
    ,
    testCase "does not parse bullshit" $
    parseNumber `notParses` "a"
  ]