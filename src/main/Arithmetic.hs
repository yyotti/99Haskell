module Arithmetic where
  {-
  - 2 Problem 31
  - (**) Determine whether a given integer number is prime.
  -
  - Example:
  -
  - * (is-prime 7)
  - T
  - Example in Haskell:
  -
  - P31> isPrime 7
  - True
  -}
  isPrime :: Integer -> Bool
  isPrime n | n < 2 = False
            | n == 2 = True
            | otherwise = all (\x -> n `mod` x /= 0) $ takeWhile (<= m) primes
    where primes = filter isPrime $ 2:[3,5..]
          m = floor $ sqrt $ (fromIntegral n :: Double)

  {-
  - 3 Problem 32
  - (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
  -
  - Example in Haskell:
  - [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
  - [9,3,3]
  -}
  myGCD :: Integer -> Integer -> Integer
  myGCD m n | m < 0 || n < 0 = myGCD (abs m) (abs n)
            | m < n = myGCD n m
            | n == 0 = m
            | otherwise = myGCD n (m `mod` n)
