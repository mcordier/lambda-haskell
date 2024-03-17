module Main (main) where

import Parser
import Evaluator

main :: IO ()
main = do
    let exprStr = "@or @true @false" -- "(LAMBDA x. x) (LAMBDA t. t) (LAMBDA e. (e e))"
    putStrLn $ evaluator exprStr
