module ListsSpec (main, spec) where

import Test.Hspec
import Lists
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problem 1" $ do
    it "returns the last element of a list (1)" $ do
      myLast [1 :: Int, 2, 3, 4] `shouldBe` 4

    it "returns the last element of a list (2)" $ do
      myLast ['x', 'y', 'z'] `shouldBe` 'z'

    it "throws an exception if used with an empty list" $ do
      myLast [] `shouldThrow` errorCall "list is empty"

  describe "Problem 2" $ do
    it "returns the last but one element of a list (1)" $ do
      myButLast [1 :: Int, 2, 3, 4] `shouldBe` 3

    it "returns the last but one element of a list (2)" $ do
      myButLast ['a'..'z'] `shouldBe` 'y'

    it "throws an exception if used with an empty list" $ do
      evaluate (myButLast []) `shouldThrow` errorCall "list is empty"

    it "throws an exception if used with a list that has one element" $ do
      evaluate (myButLast [1 :: Int]) `shouldThrow` errorCall "list has only one element"

  describe "Problem 3" $ do
    it "returns the kth element of a list (1)" $ do
      elementAt [1 :: Int, 2, 3] 2 `shouldBe` 2

    it "returns the last but one element of a list (2)" $ do
      elementAt "haskell" 5 `shouldBe` 'e'

    it "throws an exception if used with an empty list" $ do
      evaluate (elementAt [] 2) `shouldThrow` errorCall "list is empty"

    it "throws an exception if index was negative" $ do
      evaluate (elementAt [1 :: Int, 2, 3] 0) `shouldThrow` errorCall "invalid index:0"

  describe "Problem 4" $ do
    it "returns the number of elements of a list (1)" $ do
      myLength [123, 456, 789] `shouldBe` 3

    it "returns the number of elements of a list (2)" $ do
      myLength "Hello, world!" `shouldBe` 13

    it "returns the number of elements of a list (3)" $ do
      myLength [] `shouldBe` 0
