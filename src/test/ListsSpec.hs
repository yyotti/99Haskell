module ListsSpec (main, spec) where

import Test.Hspec
import Lists

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problem 1" $ do
    it "returns the last element of a list (1)" $ do
      myLast [1, 2, 3, 4] `shouldBe` 4

    it "returns the last element of a list (2)" $ do
      myLast ['x', 'y', 'z'] `shouldBe` 'z'

    it "throws an exception if used with an empty list" $ do
      myLast [] `shouldThrow` errorCall "list is empty"
