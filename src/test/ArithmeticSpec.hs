module ArithmeticSpec (main, spec) where

import Test.Hspec
import Arithmetic
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "primes" $ do
    it "first 10 terms are [2,3,5,7,11,13,17,19,23,29]" $ do
      take 10 primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

  describe "Problem 31" $ do
    it "returns true when n = 7" $ do
      isPrime 7 `shouldBe` True

    it "returns true when n = 2" $ do
      isPrime 2 `shouldBe` True

    it "returns false when n = 6" $ do
      isPrime 6 `shouldBe` False

    it "returns false when n = 1" $ do
      isPrime 1 `shouldBe` False

  describe "Problem 32" $ do
    it "returns 9 when (m, n) = (36, 63)" $ do
      myGCD 36 63 `shouldBe` 9

    it "returns 3 when (m, n) = (-3, -6)" $ do
      myGCD (-3) (-6) `shouldBe` 3

    it "returns 3 when (m, n) = (-3, 6)" $ do
      myGCD (-3) 6 `shouldBe` 3

  describe "Problem 33" $ do
    it "returns True when (m, n) = (35, 64)" $ do
      coprime 35 64 `shouldBe` True

    it "returns True when (m, n) = (109, 2)" $ do
      coprime 109 2 `shouldBe` True

    it "returns False when (m, n) = (4, 14)" $ do
      coprime 4 14 `shouldBe` False

  describe "Problem 34" $ do
    it "returns 4 when n = 10" $ do
      totient 10 `shouldBe` 4

    it "returns 6 when n = 7" $ do
      totient 7 `shouldBe` 6

  describe "Problem 35" $ do
    it "returns [3,3,5,7] when n = 315" $ do
      primeFactors 315 `shouldBe` [3, 3, 5, 7]

    it "returns [109] when n = 109" $ do
      primeFactors 109 `shouldBe` [109]

  describe "Problem 36" $ do
    it "returns [(3,2),(5,1),(7,1)] when n = 315" $ do
      primeFactorsMult 315 `shouldBe` [(3, 2), (5, 1), (7, 1)]

    it "returns [(109, 1)] when n = 109" $ do
      primeFactorsMult 109 `shouldBe` [(109, 1)]

  describe "Problem 37" $ do
    it "returns 4 when n = 10" $ do
      totient2 10 `shouldBe` 4

    it "returns 6 when n = 7" $ do
      totient2 7 `shouldBe` 6

  describe "Problem 39" $ do
    it "returns [11,13,17,19] when (s, e) = (10, 20)" $ do
      primesR 10 20 `shouldBe` [11, 13, 17, 19]

  describe "Problem 40" $ do
    it "returns (5, 23) when n = 28" $ do
      goldbach 28 `shouldBe` (5, 23)

    it "throws exception (1)" $ do
      evaluate (goldbach 27) `shouldThrow` errorCall "n must be even number"

    it "throws exception (2)" $ do
      evaluate (goldbach 1) `shouldThrow` errorCall "n must be grater than 2"
