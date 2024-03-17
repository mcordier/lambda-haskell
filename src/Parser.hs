module Parser
    ( parseLambda,
      parserLambda',
      Term(..)
    ) where

import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr

import Control.Monad.Identity (Identity)

-- Define a data type for lambda calculus expressions
data Term
    = Abstraction String Term   -- Abstraction: (LAMBDA x. T)
    | Application Term Term  -- Application: (T1 T2)
    | Var String                -- Variable: x
    deriving Show

-- Parse a lambda expression
lambdaTerm :: Parser Term
lambdaTerm = lambdaAbstraction <|> lambdaApplication <|> simple

simple :: Parser Term
simple = lambdaVar <|> paren <|> churchNum <|> encodings

lambdaAbstraction :: Parser Term
lambdaAbstraction = do
      string "LAMBDA"
      spaces
      var <- letter -- <|> char '$' <|> char '#'
      char '.'
      spaces
      body <- lambdaTerm
      return(Abstraction [var] body)

lambdaApplication :: Parser Term
lambdaApplication = do
  t1 <- simple
  spaces
  t2 <- many lambdaTerm <|> many simple
  return(foldl Application t1 t2)

lambdaVar :: Parser Term
lambdaVar = do
  var <- letter
  return(Var [var])

paren :: Parser Term
paren = between (char '(') (char ')') lambdaTerm

-- Parse positive integers as Church numerals
churchNum :: Parser Term
churchNum = do
  v <- many1 digit
  return(Abstraction "f" (Abstraction "x" (applyTimes (read v :: Int))))

-- Apply the successor function n times
applyTimes 0 = Var "x"
applyTimes n = Application (Var "f") (applyTimes (n-1))

encodings :: Parser Term
encodings = do
  char '@'
  datatype <- many letter
  return(myparse (case datatype of
    "succ"    -> "(LAMBDA n. (LAMBDA f. (LAMBDA x. f (n f x))))"
    "pred"    -> "(LAMBDA n. LAMBDA f. LAMBDA x. n (LAMBDA g. LAMBDA h. h (g f)) (LAMBDA u. x) (LAMBDA u. u))"
    "plus"    -> "(LAMBDA m. (LAMBDA n. (LAMBDA f. (LAMBDA x. ((m f) (n (f x)))))))"
    "sub"     -> "(LAMBDA m. LAMBDA n. LAMBDA s. LAMBDA z. (m s) (n (s z)))"
    "true"    -> "(LAMBDA t. (LAMBDA f. t))"
    "false"   -> "(LAMBDA t. (LAMBDA f. f))"
    "and"     -> "(LAMBDA p. LAMBDA q. (p q) p)"
    "or"      -> "(LAMBDA p. LAMBDA q. (p p) q)"
    "not"     -> "(LAMBDA p. (p @false) @true)"
   ))

-- Parse a lambda expression from a string
parseLambda :: String -> Either ParseError Term
parseLambda = parse lambdaTerm ""

myparse :: String -> Term
myparse str = case (parse lambdaTerm "" str) of
    Left msg -> error $ show msg
    Right term' -> term'

parserLambda' :: String -> String
parserLambda' x = case parseLambda x of
    Left err -> error $ show err
    Right expr -> show expr

