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
