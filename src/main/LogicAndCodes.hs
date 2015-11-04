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
  xor' a b = or' (and' a (not' b)) (and' (not' a) b)

  impl' :: Bool -> Bool -> Bool
  impl' a b = or' (not' a) b

  equ' :: Bool -> Bool -> Bool
  equ' a b = not' $ xor' a b

  table :: (Bool -> Bool -> Bool) -> String
  table f = intercalate "\n" $ map (\(x, y) -> intercalate " " $ map (\z -> show z) [x, y, f x y]) bools
    where bools = concatMap (\x -> map (\y -> (x, y)) [True, False]) [True, False]

  {-
  - 3 Problem 47
  - (*) Truth tables for logical expressions (2).
  -
  - Continue problem P46 by defining and/2, or/2, etc as being operators.
  - This allows to write the logical expression in the more natural way, as in the example:
  - A and (A or not B).
  - Define operator precedence as usual; i.e. as in Java.
  -
  - Example in Haskell:
  - > table2 (\a b -> a `and'` (a `or'` not b))
  - True True True
  - True False True
  - False True False
  - False False False
  -}
  table2 :: (Bool -> Bool -> Bool) -> String
  table2 f = table f

  {-
  - 4 Problem 48
  - (**) Truth tables for logical expressions (3).
  -
  - Generalize problem P47 in such a way that the logical expression may contain any number of logical variables.
  - Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains
  - the logical variables enumerated in List.
  -
  - Example in Haskell:
  - > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
  - -- infixl 3 `equ'`
  - True  True  True  True
  - True  True  False True
  - True  False True  True
  - True  False False True
  - False True  True  True
  - False True  False True
  - False False True  True
  - False False False True
  -
  -  -- infixl 7 `equ'`
  -  True  True  True  True
  -  True  True  False True
  -  True  False True  True
  -  True  False False False
  -  False True  True  False
  -  False True  False False
  -  False False True  False
  -  False False False False
  -}
  tablen :: Int -> ([Bool] -> Bool) -> String
  tablen n f = intercalate "\n" $ map (\bs -> intercalate " " $ map toStr $ bs ++ [f bs]) $ bools n
    where bools m | m == 0 = [[]]
                  | otherwise = concatMap (\bs -> map (\b -> bs ++ [b]) [True, False]) $ bools (m - 1)
          toStr b | b == True = "True "
                  | otherwise = "False"

  {-
  - 5 Problem 49
  - (**) Gray codes.
  -
  - An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
  -
  - n = 1: C(1) = ['0','1'].
  - n = 2: C(2) = ['00','01','11','10'].
  - n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
  - Find out the construction rules and write a predicate with the following specification:
  -
  - % gray(N,C) :- C is the N-bit Gray code
  -
  - Can you apply the method of "result caching" in order to make the predicate more efficient,
  - when it is to be used repeatedly?
  -
  - Example in Haskell:
  - P49> gray 3
  - ["000","001","011","010","110","111","101","100"]
  -}
  gray :: Int -> [String]
  gray 1 = ["0", "1"]
  gray n = add0 ++ add1
    where add0 = map (\g -> "0" ++ g) g'
          add1 = map (\g -> "1" ++ g) $ reverse g'
          g' = gray (n - 1)
