module Schemeinterpreter
where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Maybe



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex Double Double
             deriving (Show, Eq, Ord)

tryString = try . string             

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

parseCharacter :: Parser LispVal
parseCharacter = do
                  tryString "#\\"
                  c <- (tryString "newline" >> return '\n') <|> 
                       (tryString "space" >> return ' ') <|>
                       anyChar
                  return $ Character c

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

-- applicative style bitches :)
-- I'm really happy that I just learned it hwo to use it in real project
parseFloat :: Parser LispVal
parseFloat = (\x _ y _ -> Float (fst.head$readFloat ("0"++x++"."++y))) 
  <$> many digit
  <*> char '.' 
  <*> many digit
  <*> optional (oneOf "sfdlSFDL")

parseRatio :: Parser LispVal
parseRatio = (\x _ y -> Ratio (read x % read y)) 
  <$> many1 digit
  <*> char '/' 
  <*> many1 digit

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

sign :: Char -> Double
sign '+' = 1
sign '-' = -1

-- lets try monad style again
parseComplex :: Parser LispVal
parseComplex = do s1 <- optionMaybe $ char '-'
                  r <- (try parseFloat <|> parseNumber)
                  s2 <- oneOf "-+"
                  i <- (try parseFloat <|> parseNumber)
                  char 'i'
                  return $ Complex (sign (fromMaybe '+' s1) * toDouble r) (sign s2 * toDouble i)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


--parseExpr :: Parser LispVal
--parseExpr = parseAtom
--            <|> parseString
--            <|> try parseNumber -- we need the 'try' because 
--            <|> try parseCharacter -- these can all start with the hash char
--            <|> try parseFloat
--            <|> try parseRatio

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseNumber -- we need the 'try' because 
            <|> try parseCharacter -- these can all start with the hash char
            <|> try parseFloat
            <|> try parseRatio
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)

