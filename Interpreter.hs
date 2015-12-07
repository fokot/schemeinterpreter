{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
eval val@(Character _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _ _) = return val
eval val@(Vector _) = return val
eval (List [Atom "quote", val]) = return val
-- as with standard Scheme, our evaluator considers #f to be false and any other value to be true
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List ((Atom "cond") : alts)) = cond alts             
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
              --("eq?", eq),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", unaryBoolOp booleanQ),
              --("pair?", unaryBoolOp pairQ),
              ("list?", unaryBoolOp listQ),
              ("symbol?", unaryBoolOp symbolQ),
              ("char?", unaryBoolOp charQ),
              ("string?", unaryBoolOp stringQ),
              ("vector?", unaryBoolOp vectorQ),
              ("number?", unaryBoolOp numberQ),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
              ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f = f . head

unaryBoolOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
unaryBoolOp f = return . Bool . f . head

booleanQ, {-pairQ,-} listQ, symbolQ, charQ, stringQ, vectorQ, numberQ :: LispVal -> Bool
booleanQ (Bool _) = True
booleanQ _ = False
--pairQ _ = False
listQ (List _) = True
listQ (DottedList _ _) = True
listQ _ = False
symbolQ (Atom _) = True
symbolQ _ = False
charQ (Character _) = True
charQ _ = undefined
stringQ (String _) = True
stringQ _ = False
vectorQ (Vector _) = True
vectorQ _ = False
numberQ (Number _) = True
numberQ _ = False

symbol2string, string2symbol :: LispVal -> ThrowsError LispVal
symbol2string (Atom s)   = return $ String s
symbol2string e          = throwError $ TypeMismatch "symbol" e
string2symbol (String s) = return $ Atom s
string2symbol e          = throwError $ TypeMismatch "string" e

eq :: [LispVal] -> ThrowsError LispVal
eq []          = throwError $ NumArgs 2 []
eq args@(x:[]) = throwError $ NumArgs 2 args
eq (x:y:[])    = (==) <$> (eval x) <*> (eval y) >>= return . Bool
-- monad style
--eq (x:y:[])    = do
--    xx <- eval x
--    yy <- eval y
--    return $ Bool $ xx == yy

-- this is not working
--eq (x:tail)    = return Bool $ all (== first) $ map eval tail where first = eval x
eq xs          = let evaluated = mapM eval xs
                 in do (e:es) <- evaluated
                       return $ Bool $ all (== e) es

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> error "ev on list" -- never will be executed, cos eqv throws only NumArgs error
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = do
    a1 :: [LispVal] <- mapM eval arg1
    a2 :: [LispVal] <- mapM eval arg1
    zipped :: [LispVal] <- sequence $ zipWith (\x y -> equal [x, y]) a1 a2 :: ThrowsError [LispVal]
    return . Bool $ all (== Bool True) zipped
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

cond :: [LispVal] -> ThrowsError LispVal
cond ((List (Atom "else" : value : [])) : []) = eval value
cond ((List (condition : value : [])) : alts) = do
    result <- eval condition
    boolResult :: Bool <- unpackBool result
    if boolResult then eval value
                  else cond alts
cond ((List a) : _) = throwError $ NumArgs 2 a
cond (a : _) = throwError $ NumArgs 2 [a]
cond _ = throwError $ Default "Not viable alternative in cond"





--eval (List [Atom "if", pred, conseq, alt]) = 
--     do result <- eval pred
--        case result of
--             Bool False -> eval alt
--             otherwise  -> eval conseq

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

r args = extractValue $ trapError $ liftM show $ readExpr args >>= eval
p args = extractValue $ trapError $ liftM show $ readExpr args

