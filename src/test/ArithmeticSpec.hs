module ArithmeticSpec (main, spec) where

import Test.Hspec
import Arithmetic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problem 31" $ do
    it "returns true (1)" $ do
      isPrime 7 `shouldBe` True

    it "returns true (2)" $ do
      isPrime 2 `shouldBe` True

    it "returns false (1)" $ do
      isPrime 6 `shouldBe` False

    it "returns false (2)" $ do
      isPrime 1 `shouldBe` False

  describe "Problem 32" $ do
    it "returns GCD (1)" $ do
      myGCD 36 63 `shouldBe` 9

    it "returns GCD (2)" $ do
      myGCD (-3) (-6) `shouldBe` 3

    it "returns GCD (3)" $ do
      myGCD (-3) 6 `shouldBe` 3

  describe "Problem 33" $ do
    it "returns True (1)" $ do
      coprime 35 64 `shouldBe` True

    it "returns True (2)" $ do
      coprime 109 2 `shouldBe` True

    it "returns False (1)" $ do
      coprime 4 14 `shouldBe` False
