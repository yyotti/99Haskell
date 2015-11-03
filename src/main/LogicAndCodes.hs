module LogicAndCodes where
  import Data.List

  {-
  - 2 Problem 46
  - (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence)
  - which succeed or fail according to the result of their respective operations;
  - e.g. and(A,B) will succeed, if and only if both A and B succeed.
  -
  - A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
  -
  - Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
  -
  - Example in Haskell:
  - > table (\a b -> (and' a (or' a b)))
  - True True True
  - True False True
  - False True False
  - False False False
  -}
  not' :: Bool -> Bool
  not' a | a == True = False
         | otherwise = True
  and' :: Bool -> Bool -> Bool
  and' a b | (a, b) == (True, True) = True
           | otherwise = False
  or' :: Bool -> Bool -> Bool
  or' a b | (a, b) == (False, False) = False
          | otherwise = True
  nand' :: Bool -> Bool -> Bool
  nand' a b = not' $ and' a b
  nor' :: Bool -> Bool -> Bool
  nor' a b = not' $ or' a b
  xor' :: Bool -> Bool -> Bool
  xor' a b | (a, b) == (True, False) = True
           | (a, b) == (False, True) = True
           | otherwise = False
  impl' :: Bool -> Bool -> Bool
  impl' a b | (a, b) == (True, False) = False
            | otherwise = True
  equ' :: Bool -> Bool -> Bool
  equ' a b | (a, b) == (True, True) = True
           | (a, b) == (False, False) = True
           | otherwise = False
  table :: (Bool -> Bool -> Bool) -> String
  table f = intercalate "\n" $ map (\(x, y) -> intercalate " " $ map (\z -> show z) [x, y, f x y]) bools
    where bools = concatMap (\x -> map (\y -> (x, y)) [True, False]) [True, False]
