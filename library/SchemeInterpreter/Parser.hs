module SchemeInterpreter.Parser
where

import SchemeInterpreter.AST
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Maybe
import Data.Array


tryString :: String -> GenParser Char st String
tryString = try . string             

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

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
  <*> many1 digit
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

parseAnyList :: Parser LispVal
parseAnyList = do
    char '('
    head <- sepEndBy parseExpr spaces1
    dottedTail <- optionMaybe (char '.' >> spaces1 >> parseExpr)
    char ')'
    return $ case dottedTail of
                  Nothing -> List head
                  Just tail -> DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]    

arrayFromList :: [a] -> Array Int a
arrayFromList x = listArray (0, length x - 1) x

parseVector :: Parser LispVal
parseVector = do
    string "#("
    v <- sepBy parseExpr spaces1
    char ')'
    return $ Vector $ arrayFromList v

parseExpr :: Parser LispVal
parseExpr = parseString
            <|> try parseNumber -- we need the 'try' because 
            <|> try parseCharacter -- these can all start with the hash char
            <|> try parseVector
            <|> try parseFloat
            <|> try parseRatio
            <|> parseQuoted
            <|> parseQuasiquoted
            <|> parseUnquoted
            <|> parseAnyList
            <|> parseAtom

readExpr :: String -> Either ParseError LispVal
readExpr = parse parseExpr "lisp"

main :: IO ()
main = do 
        (expr:_) <- getArgs
        putStrLn (case readExpr expr of 
          Left err -> "No match: " ++ show err
          Right val -> "Found value" ++ show val
          )

