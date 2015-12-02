module Interpreter
where

import AST
import Error
import Parser hiding (readExpr, main)
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad.Error

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              -- FIXME
              --("eq?", eq),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)
              --("boolean?", unaryBoolOp booleanQ),
              --("pair?", unaryBoolOp pairQ),
              -- FIXME
              --("list?", unaryBoolOp listQ),
              --("symbol?", unaryBoolOp symbolQ),
              --("char?", unaryBoolOp charQ),
              --("string?", unaryBoolOp stringQ),
              --("vector?", unaryBoolOp vectorQ),
              --("number?", unaryBoolOp numberQ),
              --("symbol->string", unaryOp symbol2string),
              --("string->symbol", unaryOp string2symbol)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f = f . head

unaryBoolOp :: (LispVal -> Bool) -> [LispVal] -> LispVal
unaryBoolOp f = Bool . f . head

booleanQ, {-pairQ,-} listQ, symbolQ, charQ, stringQ, vectorQ, numberQ :: LispVal -> Bool
booleanQ (Bool _) = True
booleanQ _ = False
--pairQ _ = False
listQ (List _) = True
listQ (DottedList _ _) = True
listQ _ = False
symbolQ (Atom _) = False
symbolQ _ = False
charQ (Character _) = True
charQ _ = False
stringQ (String _) = True
stringQ _ = False
vectorQ (Vector _) = True
vectorQ _ = False
numberQ (Number _) = True
numberQ _ = False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

--eq :: [LispVal] -> ThrowsError LispVal
--eq args@(x:[]) = throwError $ NumArgs 2 args
--eq (x:y:[])    = return Bool $ eval x == eval y
---- FIXME
----eq (x:tail)    = return Bool $ all (== first) $ map eval tail where first = eval x
--eq []          = throwError $ NumArgs 2 []

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled