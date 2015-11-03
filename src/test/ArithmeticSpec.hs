module ArithmeticSpec (main, spec) where

import Test.Hspec
import Arithmetic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
