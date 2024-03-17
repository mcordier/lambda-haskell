module Evaluator
    ( evaluator
    , isVal
    ) where

import Data.List
import Parser (Term(..), parseLambda)
import Control.Exception
import Data.Typeable

type Info = Integer

type Context = [(String, Binding)]

data Binding = NameBind deriving Show

data NlTerm = NlVar Info Integer Integer
 | NlAbs Info String NlTerm
 | NlApp Info NlTerm NlTerm
 deriving (Show, Eq)

termToString :: Context -> NlTerm -> String
termToString ctx (NlAbs info x t1) = 
    let (x', ctx') = freshName x ctx 
    in "(LAMBDA " ++ x' ++ ". " ++ termToString ctx' t1 ++ ")"
termToString ctx (NlApp info t1 t2) = termToString ctx t1 ++ " " ++termToString ctx t2
termToString ctx (NlVar info x n) = fst (getIndexElement ctx (fromIntegral x))

termShift :: Integer -> NlTerm -> NlTerm
termShift d t = 
    let
        walk :: Integer -> NlTerm -> NlTerm
        walk c (NlVar info x n) = 
            if x >= c then (NlVar info (x + d) (n + d)) else (NlVar info x (n + d))
        walk c (NlAbs info x t1) = NlAbs info x (walk (c + 1) t1)
        walk c (NlApp info t1 t2) = NlApp info (walk c t1) (walk c t2)

    in
        walk 0 t

termSubst :: Integer -> NlTerm -> NlTerm -> NlTerm
termSubst j s t =
    let
        walk :: Integer -> NlTerm -> NlTerm
        walk c (NlVar info x n) = 
            if x==j+c then termShift c s else NlVar info x n
        walk c (NlAbs info x t1) = NlAbs info x (walk (c+1) t1)
        walk c (NlApp info t1 t2) = NlApp info (walk c t1) (walk c t2)

    in
        walk 0 t

termSubstTop :: NlTerm -> NlTerm -> NlTerm
termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: NlTerm -> Bool 
isVal (NlAbs _  _  _) = True
isVal _ = False

eval' :: Context -> NlTerm -> NlTerm
eval' ctx t = case t of
    NlApp info (NlAbs _ x t12) v2 | isVal v2 ->
        termSubstTop v2 t12
    NlApp info v1 t2 | isVal v1 ->
        let t2' = eval' ctx t2 in
        NlApp info v1 t2'
    NlApp info t1 t2 ->
        let t1' = eval' ctx t1 in
        NlApp info t1' t2
    -- NlAbs info x t -> NlAbs info x (eval' ctx t)
    _ -> t 


eval :: Context -> NlTerm -> NlTerm
eval ctx t = let t' = (eval' ctx) t
    in (if t' == t then t else eval ctx t')

---------------- Convert Term to Nameless representation

-- Function to convert a lambda term to its nameless representation
termToNlTerm :: Term -> Context -> (NlTerm, Context)
termToNlTerm (Var x) ctx =
    case lookupIndex x ctx of
        Just index -> (NlVar 0 (toInteger index) (toInteger $ length ctx), ctx)
        Nothing -> error "Variable not found in context"
termToNlTerm (Abstraction x body) ctx =
    let newCtx = (x, NameBind) : ctx
        (newBody, updatedCtx) = termToNlTerm body newCtx
    in (NlAbs 0 x newBody, updatedCtx)
termToNlTerm (Application t1 t2) ctx =
    let (nlT1, ctx1) = termToNlTerm t1 ctx
        (nlT2, ctx2) = termToNlTerm t2 ctx1
    in (NlApp 0 nlT1 nlT2, ctx2)

-- Helper function to generate a fresh name
freshName :: String -> Context -> (String, Context)
freshName x ctx = case lookupIndex x ctx of
    Just index -> (x ++ "'", (x ++ "'", NameBind) : ctx)
    Nothing -> (x, ctx)
    

-- Helper function to lookup the index of a variable in the context
lookupIndex :: String -> Context -> Maybe Int
lookupIndex x ctx = elemIndex x (map fst ctx)

getIndexElement :: [a] -> Int -> a
getIndexElement list index
    | index < 0 = error ":'("                   -- Index is negative
    | index < length list = (head (drop index list))  -- Index is within bounds
    | otherwise = error ":'("                  -- Index is out of bounds

---------------- Complete Evaluator
evaluator :: String -> String
evaluator x = case parseLambda x of
    Left err -> show err
    Right expr -> 
        let (term, ctx) = termToNlTerm expr []
        in termToString ctx (eval ctx term)

-- evaluator2 :: String -> String
-- evaluator2 x = case parseLambda x of
--     Left err -> show err
--     Right expr -> 
--         let (term, ctx) = termToNlTerm expr []
--         in termToString ctx $ eval' ctx (eval ctx term)
