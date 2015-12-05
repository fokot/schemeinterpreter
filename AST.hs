module AST
where

import Data.Array
import Data.Ratio

data LispVal = Atom String
         | String String
         | Number Integer
         | Bool Bool
         | List [LispVal]
         | DottedList [LispVal] LispVal
         | Character Char
         | Float Double
         | Ratio Rational
         | Complex Double Double
         | Vector (Array Int LispVal)
         deriving (Eq, Ord)

instance Show LispVal where show = showVal
 
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Character c) = "#\\" ++ [c]
showVal (Float f) = show f
showVal (Complex r i) = show r ++ if i < 0 then "" else "+" ++ show i
showVal (Vector a) = "[" ++ unwordsList (elems a) ++ "]"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal