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
