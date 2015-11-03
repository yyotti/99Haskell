module Arithmetic where
  import Data.List

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

  {-
  - 4 Problem 33
  - (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
  -
  - Example in Haskell:
  - * coprime 35 64
  - True
  -}
  coprime :: Integer -> Integer -> Bool
  coprime m n = myGCD m n == 1

  {-
  - 5 Problem 34
  - (**) Calculate Euler's totient function phi(m).
  -
  - Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
  -
  - Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
  -
  - Example in Haskell:
  - * totient 10
  - 4
  -}
  totient :: Integer -> Int
  totient n = length $ filter (coprime n) [1..(n - 1)]

  {-
  - 6 Problem 35
  - (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
  -
  - Example in Haskell:
  - > primeFactors 315
  - [3, 3, 5, 7]
  -}
  primeFactors :: Integer -> [Integer]
  primeFactors n = primeFactors' n $ takeWhile (\x -> x * x <= n) primes
    where primeFactors' m [] = [m]
          primeFactors' m (p:ps) | m == 1 = []
                                 | m `mod` p == 0 = p:(primeFactors' (m `div` p) $ p:ps)
                                 | otherwise = primeFactors' m ps
          primes = filter isPrime $ 2:[3,5..]

  {-
  - 7 Problem 36
  - (**) Determine the prime factors of a given positive integer.
  -
  - Construct a list containing the prime factors and their multiplicity.
  -
  - Example in Haskell:
  - *Main> prime_factors_mult 315
  - [(3,2),(5,1),(7,1)]
  -}
  primeFactorsMult :: Integer -> [(Integer, Int)]
  primeFactorsMult n = map (\xs -> (head xs, length xs)) $ group $ primeFactors n

  {-
  - 8 Problem 37
  - (**) Calculate Euler's totient function phi(m) (improved).
  -
  - See problem 34 for the definition of Euler's totient function.
  - If the list of the prime factors of a number m is known in the form of problem 36
  - then the function phi(m) can be efficiently calculated as follows:
  - Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m.
  - Then phi(m) can be calculated with the following formula:
  -
  - phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
  -          (p2 - 1) * p2 ** (m2 - 1) *
  -          (p3 - 1) * p3 ** (m3 - 1) * ...
  -
  - Note that a ** b stands for the b'th power of a.
  -}
  totient2 :: Integer -> Integer
  totient2 n = product $ map (\x -> let (p, m) = x in (p - 1) * p^(m - 1)) $ primeFactorsMult n

  {-
  - 9 Problem 38
  - (*) Compare the two methods of calculating Euler's totient function.
  -
  - Use the solutions of problems 34 and 37 to compare the algorithms.
  - Take the number of reductions as a measure for efficiency.
  - Try to calculate phi(10090) as an example.
  -}
  -- TODO pending

  {-
  - 10 Problem 39
  - (*) A list of prime numbers.
  -
  - Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
  -
  - Example in Haskell:
  - P29> primesR 10 20
  - [11,13,17,19]
  -}
  primesR :: Integer -> Integer -> [Integer]
  primesR s e = takeWhile (<= e) $ dropWhile (< s) primes
    where primes = filter isPrime $ 2:[3,5..]
