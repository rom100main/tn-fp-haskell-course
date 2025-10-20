-- Build me with: cabal build TP4.hs
-- Execute me with: cabal run -v0 TP4.hs
-- Load me in the REPL with: cabal repl TP4.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import System.Process
import Test.QuickCheck

-- The goal of this TP is to implement an evaluator for arithmetic expressions.
-- Here is the incremental list of objectives:
-- - Define a type of arithmetic expressions. It should support:
--   - Int constants
--   - Additions of two expressions
--   - Negation of an expression
--   - Feel free to add constructs
-- - Define an evaluator for an expression. I.e. given your type of expressions
--   Expr, define the function eval :: Expr -> Int
-- - Make 'Expr' an instance of the 'Show' typeclass
-- - Use this instance to compare your implementation of 'eval' with
--   the result of evaluating your expressions with python3, using the 'pyEval'
--   function below. Write a quickCheck test to compare the two implementations.

data OpType
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Enum, Show)

data Expr
  = Const Int
  | Op OpType Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Const n) = n
eval (Op Add e1 e2) = eval e1 + eval e2
eval (Op Sub e1 e2) = eval e1 - eval e2
eval (Op Mul e1 e2) = eval e1 * eval e2
eval (Op Div e1 e2) = eval e1 `div` eval e2

-- | Parse a string into an Expr.
stringToExpr :: String -> Either String Expr
stringToExpr s = parseExpr (words s)
  where
    parseExpr :: [String] -> Either String Expr
    parseExpr [] = Right (Const 0)
    parseExpr [n] = case reads n :: [(Int, String)] of
      [(val, "")] -> Right (Const val)
      _ -> Left ("Invalid constant: " ++ n)
    parseExpr xs =
      case break (`elem` ["*", "/"]) xs of
        (left, op : right) | op `elem` ["*", "/"] -> do
          leftExpr <- parseExpr left
          rightExpr <- parseExpr right
          let operator = if op == "*" then Mul else Div
          Right (Op operator leftExpr rightExpr)
        _ ->
          case break (`elem` ["+", "-"]) xs of
            (left, op : right) | op `elem` ["+", "-"] -> do
              leftExpr <- parseExpr left
              rightExpr <- parseExpr right
              let operator = if op == "+" then Add else Sub
              Right (Op operator leftExpr rightExpr)
            _ -> Left "Invalid expression"

-- | pyEval calls python3 and make it execute a python statement.
-- For example, pyEval "2 * 3" returns "6"
-- For 'readProcess' documentation, see:
-- https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#v:readProcess
--
-- Use 'monadicIO' to use 'pyEval' in a QuickCheck property:
-- https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Monadic.html#v:monadicIO
--
-- Use https://hoogle.haskell.org/ to find the functions you need
pyEval :: String -> IO String
pyEval expr = do
  readProcess "python3" ["-c", "print(" ++ expr ++ ", end='')"] ""

main :: IO ()
main = do
  pyResult <- pyEval "1 + 3"
  putStrLn ("pyEval \"1 + 3\" returned: " ++ pyResult)
  let expr = Op Add (Const 1) (Const 3)
  let result = eval expr
  putStrLn ("eval of " ++ show expr ++ " returned: " ++ show (result))
  let exprStr = "1 + 5 * 3"
  let parsedExpr = stringToExpr exprStr
  putStrLn ("stringToExpr \"" ++ exprStr ++ "\" returned: " ++ show parsedExpr)
