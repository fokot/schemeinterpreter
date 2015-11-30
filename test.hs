module Schemeinterpreter.Test
where

import Test.Tasty
import Test.Tasty.HUnit
import Schemeinterpreter hiding (main)
import Text.ParserCombinators.Parsec (parse)
import Text.Parsec.Prim
import Data.Functor.Identity
import Data.Ratio

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ parseNumerUnitTests,
                            parseCharacterUnitTests,
                            parseFloatUnitTests,
                            parseRatioUnitTests,
                            parseComplexUnitTests
                          ]

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


parseCharacterUnitTests = testGroup "parseCharacter Unit tests"
  [ testCase "parses lower case letter" $
    parseCharacter `parses` "#\\a" @?= Character 'a'
    ,
    testCase "parses upper case letter" $
    parseCharacter `parses` "#\\A" @?= Character 'A'
    ,
    testCase "parses digit" $
    parseCharacter `parses` "#\\1" @?= Character '1'
    ,
    testCase "parses left parenthesis" $
    parseCharacter `parses` "#\\(" @?= Character '('
    ,
    testCase "parses the space character" $
    parseCharacter `parses` "#\\ " @?= Character ' '
    ,
    testCase "parses the preferred way to write a space" $
    parseCharacter `parses` "#\\space" @?= Character ' '
    ,
    testCase "parses the newline character" $
    parseCharacter `parses` "#\\newline" @?= Character '\n'
    ,
    testCase "does not parse bullshit" $
    parseCharacter `notParses` "a"
    ,
    testCase "does not parse bullshit" $
    parseExpr `parses` "#\\bullshit" @?= Character '\n'
  ]

parseFloatUnitTests = testGroup "parseFloat Unit tests"
  [ testCase "parses float number with dot" $
    parseFloat `parses` "1.2" @?= Float 1.2
    ,
    testCase "parses float number with dot and short precision" $
    parseFloat `parses` "1.2s" @?= Float 1.2
    ,
    testCase "parses float number with dot and short precision in upper case" $
    parseFloat `parses` "1.2S" @?= Float 1.2
    ,
    testCase "parses float number with dot but without significand" $
    parseFloat `parses` ".2" @?= Float 0.2
    ,
    testCase "parses float number with dot but without significand with single precision" $
    parseFloat `parses` ".2s" @?= Float 0.2
    ,
    testCase "parses float number without fraction" $
    parseFloat `parses` "2." @?= Float 2
    ,
    testCase "parses float number with dot and single precision" $
    parseFloat `parses` "1.2f" @?= Float 1.2
    ,
    testCase "parses float number with dot and double precision" $
    parseFloat `parses` "1.2d" @?= Float 1.2
    ,
    testCase "parses float number with dot and long precision" $
    parseFloat `parses` "1.2l" @?= Float 1.2
    ,
    testCase "does not parse bullshit" $
    parseFloat `notParses` "sss"
  ]

parseRatioUnitTests = testGroup "parseRatio Unit tests"
  [ testCase "parses ratio" $
    parseRatio `parses` "1/2" @?= (Ratio $ 1 % 2)
    ,
    testCase "does not parse bullshit" $
    parseRatio `notParses` "1/.3"
    ,
    testCase "does not parse bullshit" $
    parseRatio `notParses` "sss"
  ]

parseComplexUnitTests = testGroup "parseComplex Unit tests"
  [ testCase "parses comlex" $
    parseComplex `parses` "1+2i" @?= Complex 1 2
    ,
    testCase "parses comlex with minus sign at real part" $
    parseComplex `parses` "-1+2i" @?= Complex (-1) 2
    ,
    testCase "parses comlex with minus sign at imaginary part" $
    parseComplex `parses` "1-2i" @?= Complex 1 (-2)
    ,
    testCase "parses comlex with real number at real part" $
    parseComplex `parses` "1.5+2i" @?= Complex 1.5 2
    ,
    testCase "parses comlex with real number at imaginary part" $
    parseComplex `parses` "1+2.3i" @?= Complex 1 2.3
    ,
    testCase "parses comlex with real numbers at both parts" $
    parseComplex `parses` "1.1+2.1i" @?= Complex 1.1 2.1
    ,
    testCase "does not parse bullshit" $
    parseComplex `notParses` "1+d"
    ,
    testCase "does not parse bullshit" $
    parseComplex `notParses` "sss"
  ]

