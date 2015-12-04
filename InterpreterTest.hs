module ParserTest
where

import Test.Tasty
import Test.Tasty.HUnit
import Interpreter hiding (main, readExpr)
import Parser hiding (main)


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ simpleNumericUnitTests,
                            typeTestUnitTests,
                            functionsUnitTests
                          ]

evaluatesTo :: String -> String -> Assertion
expression `evaluatesTo` value = 
  case readExpr expression of
    Left err -> assertString $ "Can not parse:\n" ++ expression ++ "\n" ++ show err
    Right parsed -> case eval parsed of
                    Left err -> assertString $ "Can not evaluate:\n" ++ expression ++ "\n" ++ show err
                    Right evaluated -> show evaluated @?= value

simpleNumericUnitTests = testGroup "simple numeric Unit tests"
  [ testCase "plus" $
    "(+ 2 2)" `evaluatesTo` "4"
    ,
    testCase "take first from the list" $
    "(+ 2 (-4 1))" `evaluatesTo` "2"
    ,
    testCase "nested expressions" $
    "(+ 2 (- 4 1))" `evaluatesTo` "5"
    ,
    testCase "nested expressions + list of arguments" $
    "(- (+ 4 6 3) 3 5 2)" `evaluatesTo` "3"
  ]

typeTestUnitTests = testGroup "type test Unit tests"
  [ testCase "(boolean? #t) passes" $
    "(boolean? #t)" `evaluatesTo` "#t"
    ,
    testCase "(boolean? #f) passes" $
    "(boolean? #f)" `evaluatesTo` "#t"
    ,
    testCase "(boolean? aaa) fails" $
    "(boolean? 'aaa)" `evaluatesTo` "#f"
    ,
    -- pairs vs list, pairs are not nil terminated, lists are
    -- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.2
    --testCase "(pair? '(a . b)) passes" $
    --"(pair? '(a . b))" `evaluatesTo` "#t"
    --,
    --testCase "(pair? '(a b c)) passes" $
    --"(pair? '(a b c))" `evaluatesTo` "#t"
    --,
    --testCase "(pair? '()) passes" $
    --"(pair? '())" `evaluatesTo` "#t"
    --,
    --testCase "pair? passes dotted syntax" $
    --"(pair? '(a . b))" `evaluatesTo` "#t"
    --,
    --testCase "(pair? '#(a b)) passes" $
    --"(pair? '#(a b))" `evaluatesTo` "#t"
    --,
    --testCase "(pair? aaa) fails" $
    --"(pair? aaa)" `evaluatesTo` "#f"
    --,
    testCase "(list? '(a b c)) passes" $
    "(list? '(a b c)" `evaluatesTo` "#t"
    ,
    testCase "(list? '()) passes" $
    "(list? '())" `evaluatesTo` "#t"
    ,
    testCase "(list? '(a . b)) passes" $
    "(list? '(a . b))" `evaluatesTo` "#t"
    ,
    testCase "(list? '(a b c)) passes" $
    "(list? '(a b c))" `evaluatesTo` "#t"
    ,
    testCase "(list? (cons 1 (cons 2 nil))) passes" $
    "(list? (cons 1 (cons 2 nil)))" `evaluatesTo` "#t"
    ,
    testCase "(list? (+ 1 2)) fails" $
    "(list? (+ 1 2))" `evaluatesTo` "#f"
    ,
    testCase "(symbol? 'foo) passes" $
    "(symbol? 'foo)" `evaluatesTo` "#t"
    ,
    testCase "(symbol? (car '(a b))) passes" $
    "(symbol? (car '(a b)))" `evaluatesTo` "#t"
    ,
    testCase "(symbol? \"bar\") fails" $
    "(symbol? \"bar\")" `evaluatesTo` "#f"
    ,
    testCase "(symbol? 'nil) passes" $
    "(symbol? 'nil)" `evaluatesTo` "#t"
    ,
    testCase "(symbol? '()) fails" $
    "(symbol? '())" `evaluatesTo` "#f"
    ,
    testCase "(symbol? #f) fails" $
    "(symbol? #f)" `evaluatesTo` "#f"
    ,
    testCase "(char? #\\a) passes" $
    "(char? #\\a)" `evaluatesTo` "#t"
    ,
    testCase "(char? #\\space) passes" $
    "(char? #\\space)" `evaluatesTo` "#t"
    ,
    testCase "(char? a) fails" $
    "(char? a)" `evaluatesTo` "#f"
    ,
    testCase "(string? \"aaa\") passes" $
    "(string? \"aaa\")" `evaluatesTo` "#t"
    ,
    testCase "(string? a) fails" $
    "(string? a)" `evaluatesTo` "#f"
    ,
    testCase "(vector? #(0 (2 2 2 2) \"Anna\")) passes" $
    "(vector? #(0 (2 2 2 2) \"Anna\"))" `evaluatesTo` "#t"
    ,
    testCase "(vector? '(a b)) fails" $
    "(vector? '(a b))" `evaluatesTo` "#f"
  ]

functionsUnitTests = testGroup "functions Unit tests"
  [ testCase "(eq? 5 (+ 1 3 1))" $
    "(eq? 5 (+ 1 3 1))" `evaluatesTo` "#t"
    ,
    testCase "(eq? 5 (+ 1 3 1 2))" $
    "(eq? 5 (+ 1 3 1 2))" `evaluatesTo` "#f"
    ,
    testCase "(eq? 5 (+ 1 3 1) (- 6 1))" $
    "(eq? 5 (+ 1 3 1) (- 6 1))" `evaluatesTo` "#t"
    ,
    testCase "(eq? 5 (+ 1 3 1) (- 6 2))" $
    "(eq? 5 (+ 1 3 1) (- 6 2))" `evaluatesTo` "#f"
    ,
    testCase "(> 2 3)" $
    "(> 2 3)" `evaluatesTo` "#f"
    ,
    testCase "(< 2 3)" $
    "(< 2 3)" `evaluatesTo` "#t"
    ,
    testCase "(if (> 2 3) \"yes\" \"no\")" $
    "(if (> 2 3) \"yes\" \"no\")" `evaluatesTo` "\"no\""
    ,
    testCase "(if (> 2 3) \"no\" \"yes\")" $
    "(if (< 2 3) \"yes\" \"no\")" `evaluatesTo` "\"yes\""
  ]
