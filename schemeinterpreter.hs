module Schemeinterpreter
where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show, Eq, Ord)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChars <|> noneOf "\"\\"
                char '"'
                return $ String x

escapedChars :: Parser Char
escapedChars = do 
                char '\\' 
                c <- oneOf "\"nrt\\"
                return $ case c of 
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    c -> c

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

readBin :: String -> Integer
readBin = r 0
          where r acc [] = acc
                r acc ('0':xs) = r (acc * 2) xs
                r acc ('1':xs) = r (acc * 2 + 1) xs

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0

tryString = try . string

parseNumber :: Parser LispVal
parseNumber =  (liftM (Number . read) $ many1 digit) <|>
               (liftM (Number . readBin) $ tryString "#b" >> many1 (oneOf "01")) <|>
               (liftM (Number . oct2dig) $ tryString "#o" >> many1 octDigit) <|>
               (liftM (Number . read) $ tryString "#d" >> many1 digit) <|>
               (liftM (Number . hex2dig) $ tryString "#x" >> many1 hexDigit)

-- Excercise 1.1
--parseNumber = do 
--  s <- many1 digit
--  let i = read s :: Integer
--  return (Number i)

--parseNumber = many1 digit >>= \s -> (return . Number . read) s



parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

--main :: IO ()
--main = do 
--         (expr:_) <- getArgs
--         putStrLn (readExpr expr)

