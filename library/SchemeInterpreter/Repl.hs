module SchemeInterpreter.Repl where

import SchemeInterpreter.Error (extractValue, trapError)
import SchemeInterpreter.Interpreter  (eval, readExpr)
import Control.Monad (liftM)
import System.IO
import System.Environment (getArgs)

-- prints out a string and immediately flushes the stream
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- prints out a prompt and reads in a line of input:
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- pull the code to parse and evaluate a string and trap the errors out of main into its own function
evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

-- evaluates a string and prints the result
evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

-- read input, perform a function, and print the output, all in an infinite loop
-- the built-in function interact almost does what we want, but sequence . repeat . interact, we'd get an infinite loop
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint $ args !! 0
            otherwise -> putStrLn "Program takes only 0 or 1 argument"