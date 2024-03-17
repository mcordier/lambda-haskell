module Main (main) where

import System.IO (hFlush, stdout)
import Parser
import Evaluator

main :: IO ()
main = do
    putStrLn "Lambda Calculus Interpreter"
    putStrLn "Enter a Lambda Calculus expression or type 'quit' to exit:"
    repl

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    exprStr <- getLine
    if exprStr == "quit"
        then putStrLn "Exiting..."
        else do
            putStrLn $ "Result: " ++ evaluator exprStr
            repl

-- evaluateExpression :: String -> String
-- evaluateExpression exprStr =
--     case evaluator exprStr of
--         Left err -> "Error: " ++ show err
--         Right result -> result

-- parseAndEvaluate :: String -> Either String String
-- parseAndEvaluate exprStr =
--     case parseLambda exprStr of
--         Left err -> Left $ "Parsing error: " ++ show err
--         Right expr -> Right $ evaluator expr
