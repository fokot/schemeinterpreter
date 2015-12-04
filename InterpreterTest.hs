module ParserTest
where

import Test.Tasty
import Test.Tasty.HUnit
import Interpreter hiding (main, readExpr)
import Parser hiding (main)
import Error
import AST


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ simpleNumericUnitTests,
                            typeTestUnitTests,
                            functionsUnitTests,
                            listPrimitivesUnitTests
                          ]

evaluatesTo :: String -> String -> TestTree
expression `evaluatesTo` value = testCase expression $
  case readExpr expression of
    Left err -> assertString $ "Can not parse:\n" ++ expression ++ "\n" ++ show err
    Right parsed -> case eval parsed of
                    Left err -> assertString $ "Can not evaluate:\n" ++ expression ++ "\n" ++ show err
                    Right evaluated -> show evaluated @?= value

throwsError :: String -> LispError -> TestTree
expression `throwsError` value = testCase expression $
  case readExpr expression of
    Left err -> assertString $ "Can not parse:\n" ++ expression ++ "\n" ++ show err
    Right parsed -> case eval parsed of
                    Left err -> err @?= value
                    Right evaluated -> assertString $ "Expression " ++ expression ++ " was evaluated succesfully to " ++ (show evaluated)

simpleNumericUnitTests = testGroup "simple numeric Unit tests"
  [ 
    "(+ 2 2)" `evaluatesTo` "4"
    ,
    -- take first from the list
    "(+ 2 (-4 1))" `evaluatesTo` "2"
    ,
    -- nested expressions
    "(+ 2 (- 4 1))" `evaluatesTo` "5"
    ,
    -- nested expressions + list of arguments
    "(- (+ 4 6 3) 3 5 2)" `evaluatesTo` "3"
  ]

typeTestUnitTests = testGroup "type test Unit tests"
  [ "(boolean? #t)" `evaluatesTo` "#t"
    ,
    "(boolean? #f)" `evaluatesTo` "#t"
    ,
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
    "(list? '(a b c)" `evaluatesTo` "#t"
    ,
    "(list? '())" `evaluatesTo` "#t"
    ,
    "(list? '(a . b))" `evaluatesTo` "#t"
    ,
    "(list? '(a b c))" `evaluatesTo` "#t"
    ,
    "(list? (cons 1 (cons 2 nil)))" `evaluatesTo` "#t"
    ,
    "(list? (+ 1 2))" `evaluatesTo` "#f"
    ,
    "(symbol? 'foo)" `evaluatesTo` "#t"
    ,
    "(symbol? (car '(a b)))" `evaluatesTo` "#t"
    ,
    "(symbol? \"bar\")" `evaluatesTo` "#f"
    ,
    "(symbol? 'nil)" `evaluatesTo` "#t"
    ,
    "(symbol? '())" `evaluatesTo` "#f"
    ,
    "(symbol? #f)" `evaluatesTo` "#f"
    ,
    "(char? #\\a)" `evaluatesTo` "#t"
    ,
    "(char? #\\space)" `evaluatesTo` "#t"
    ,
    "(char? a)" `evaluatesTo` "#f"
    ,
    "(string? \"aaa\")" `evaluatesTo` "#t"
    ,
    "(string? a)" `evaluatesTo` "#f"
    ,
    "(vector? #(0 (2 2 2 2) \"Anna\"))" `evaluatesTo` "#t"
    ,
    "(vector? '(a b))" `evaluatesTo` "#f"
  ]

functionsUnitTests = testGroup "functions Unit tests"
  [ "(eq? 5 (+ 1 3 1))" `evaluatesTo` "#t"
    ,
    "(eq? 5 (+ 1 3 1 2))" `evaluatesTo` "#f"
    ,
    "(eq? 5 (+ 1 3 1) (- 6 1))" `evaluatesTo` "#t"
    ,
    "(eq? 5 (+ 1 3 1) (- 6 2))" `evaluatesTo` "#f"
    ,
    "(> 2 3)" `evaluatesTo` "#f"
    ,
    "(< 2 3)" `evaluatesTo` "#t"
    ,
    "(string=? \"test\"  \"test\")" `evaluatesTo` "#t"
    ,
    "(string=? \"test\"  \"different_test\")" `evaluatesTo` "#f"
    ,
    "(string<? \"abc\" \"bba\")" `evaluatesTo` "#t"
    ,
    "(string<? \"cbc\" \"bba\")" `evaluatesTo` "#f"
    ,
    "(if (> 2 3) \"yes\" \"no\")" `evaluatesTo` "\"no\""
    ,
    "(if (< 2 3) \"yes\" \"no\")" `evaluatesTo` "\"yes\""
    ,
    "(eq? 5 (+ 1))" `throwsError` NumArgs 2 [Number 1]
  ]

listPrimitivesUnitTests = testGroup "list primitives Unit tests"
  [ "(car '(a b c))" `evaluatesTo` "a"
    ,
    "(car '(a))" `evaluatesTo` "a"
    ,
    "(car '(a b . c))" `evaluatesTo` "a"
    ,
    "(car 'a)" `throwsError` TypeMismatch "pair" (Atom "a")
    ,
    "(car 'a 'b)" `throwsError` NumArgs 1 [Atom "a", Atom "b"]
    ,
    "(cdr '(a b c))" `evaluatesTo` "(b c)"
    ,
    "(cdr '(a b))" `evaluatesTo` "(b)"
    ,
    "(cdr '(a))" `evaluatesTo` "()"
    ,
    "(cdr '(a . b))" `evaluatesTo` "b"
    ,
    "(cdr '(a b . c))" `evaluatesTo` "(b . c)"
    ,
    "(cdr 'a)" `throwsError` TypeMismatch "pair" (Atom "a")
    ,
    "(cdr 'a 'b)" `throwsError` NumArgs 1 [Atom "a", Atom "b"]
    ,
    "(eq? '(b c) (cdr '(a b c)))" `evaluatesTo` "#t"
    ,
    "(eq? '(a b) (cdr '(a b c)))" `evaluatesTo` "#f"
    ,
    "(eq? '(b c) (cdr '(a b c d)))" `evaluatesTo` "#f"
    ,
    "(eqv? '(b c) (cdr '(a b c)))" `evaluatesTo` "#t"
    ,
    "(eqv? '(a b) (cdr '(a b c d)))" `evaluatesTo` "#f"
    ,
    "(eqv? '(a b) (cdr '(a b c d) 'c)))" `throwsError` NumArgs 1 [List [Atom "a",Atom "b",Atom "c",Atom "d"],Atom "c"]
  ]
