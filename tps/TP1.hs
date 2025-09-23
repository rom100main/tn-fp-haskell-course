-- Build me with: cabal build TP1.hs
-- Execute me with: cabal run -v0 TP1.hs
-- Load me in the REPL with: cabal repl TP1.hs, then use :r to reload the code upon changing

--
-- In this file, you need to replace the @undefined@ calls by real code.
--
-- Resource for the syntax of functions: https://learnyouahaskell.github.io/syntax-in-functions.html

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Prelude hiding (and, drop, length, not, take)

-- | A type for thumb up and thumb down emojis
data ThumbType = Up | Down
  deriving (Show) -- So that the type can be printed

main :: IO ()
main = do
  putStrLn ("Haskell is " ++ (show Up))
  quickCheck propNegAnd -- Tests the property 'propNegAnd'
  quickCheck (propLength :: [Int] -> Bool) -- Specialize function, so that data can be generated
  quickCheck propSumRecSumFold

-- | Write a negation function over Bool: 'neg'
neg :: Bool -> Bool
neg a = if a then False else True

-- | Write the conjunction function over Bool: 'and'
and :: Bool -> Bool -> Bool
and a b = if a then b else False

-- | A function stating a property of 'neg' and 'and'
propNegAnd :: Bool -> Bool
propNegAnd b = neg (and b (neg b))

-- | Write a function computing the length of a list
length :: [a] -> Int
length l = case l of
  [] -> 0
  _ : xs -> 1 + length xs

-- | write a function that states a property of 'length', for any input
-- list.
propLength :: [a] -> Bool
propLength x = length x >= 0

-- | Write a function taking the first 'n' elements of a list. The function
-- should be total.
take :: Int -> [a] -> [a]
take _ [] = []
take n (x : rest)
  | n > 0 = x : take (n - 1) rest
  | otherwise = []

-- | Write a function taking the suffix of a list, after the first 'n' elements.
drop :: Int -> [a] -> [a]
drop _ [] = []
drop n (_x : rest)
  | n > 1 = drop (n - 1) rest
  | otherwise = rest

-- | Write a recursive function that sums the elements of a list
sumRec :: [Int] -> Int
sumRec [] = 0
sumRec (x : rest) = x + sumRec rest

-- | Write a non-recursive function that sums the elements of a list, using
-- the foldr function: https://hoogle.haskell.org/?hoogle=foldr
sumFold :: [Int] -> Int
sumFold l = foldr (\x acc -> x + acc) 0 l

-- | Write a function stating a relation between 'sumRec' and 'sumFold'
propSumRecSumFold :: [Int] -> Bool
propSumRecSumFold l = sumRec l == sumFold l

-- | Write the fmap instance for 'Maybe'
fmapMaybe :: (a -> b) -> (Maybe a) -> (Maybe b)
fmapMaybe _ Nothing = Nothing
fmapMaybe f (Just x) = Just (f x)

-- | Write the map instance for 'List'. Don't use the standard library's 'map' function
fmapMaybeList :: (a -> b) -> [a] -> [b]
fmapMaybeList f (x : rest) = f x : fmapMaybeList f rest
fmapMaybeList _ [] = []
