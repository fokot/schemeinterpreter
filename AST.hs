module AST
where

import Data.Array

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
         | Vector (Array Int LispVal)
         deriving (Show, Eq, Ord)
