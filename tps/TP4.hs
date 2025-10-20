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

eval :: Expr -> Either String Int
eval (Const n) = Right n
eval (Op op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case op of
    Add -> Right (v1 + v2)
    Sub -> Right (v1 - v2)
    Mul -> Right (v1 * v2)
    Div ->
      if v2 == 0
        then Left "Division by zero"
        else Right (v1 `div` v2)

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

-- Convert Expr to a Python expression string
exprToString :: Expr -> String
exprToString (Const n) = show n
exprToString (Op op e1 e2) =
  let opStr = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "//"
   in "(" ++ exprToString e1 ++ " " ++ opStr ++ " " ++ exprToString e2 ++ ")"

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

-- Arbitrary instance for OpType
instance Arbitrary OpType where
  arbitrary = elements [Add, Sub, Mul, Div]

-- Arbitrary instance for Expr
instance Arbitrary Expr where
  arbitrary = sized exprGen
    where
      exprGen 0 = Const <$> arbitrary
      exprGen n =
        oneof
          [ Const <$> arbitrary,
            Op <$> arbitrary <*> exprGen (n `div` 2 + n `mod` 2) <*> exprGen (n `div` 2)
          ]

-- Helper to check for division by zero in Expr
hasDivByZero :: Expr -> Bool
hasDivByZero (Const _) = False
hasDivByZero (Op op e1 e2) =
  eval (Op op e1 e2) == Left "Division by zero"

-- QuickCheck property to compare eval with pyEval
prop_evalMatchesPython :: Expr -> Property
prop_evalMatchesPython expr =
  not (hasDivByZero expr) ==>
    ioProperty $ do
      let exprStr = exprToString expr
      pyResult <- pyEval exprStr
      let evalResult = case eval expr of
            Right val -> show val
            Left err -> "Error: " ++ err
      return (pyResult == evalResult)

-- Run QuickCheck property
qc :: IO ()
qc = quickCheck prop_evalMatchesPython

main :: IO ()
main = do
  let exprDiv0 = Op Div (Const 1) (Const 0)
  let exprIsDiv0 = hasDivByZero exprDiv0
  putStrLn ("Expression \"" ++ show exprDiv0 ++ "\" has a division by zero : " ++ show exprIsDiv0)

  let exprStr = "0"
  pyResult <- pyEval exprStr
  putStrLn ("pyEval \"" ++ exprStr ++ "\" returned: " ++ pyResult)
  let parsedExpr = stringToExpr exprStr
  let evalResult = case parsedExpr of
        Right expr -> case eval expr of
          Right val -> show val
          Left err -> "Error: " ++ err
        Left err -> "Error: " ++ err
  putStrLn ("Evaluating parsed expression from \"" ++ exprStr ++ "\" returned: " ++ evalResult)

  putStrLn "Running QuickCheck tests..."
  qc
