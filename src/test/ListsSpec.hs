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
      myLength [123 :: Int, 456, 789] `shouldBe` 3

    it "returns the number of elements of a list (2)" $ do
      myLength "Hello, world!" `shouldBe` 13

    it "returns the number of elements of a list (3)" $ do
      myLength [] `shouldBe` 0

  describe "Problem 5" $ do
    it "returns the reversed list (1)" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"

    it "returns the reversed list (2)" $ do
      myReverse [1 :: Int, 2, 3, 4] `shouldBe` [4, 3, 2, 1]

    it "returns the reversed list (3)" $ do
      myReverse ([] :: [Int]) `shouldBe` []

  describe "Problem 6" $ do
    it "returns false" $ do
      isPalindrome [1 :: Int, 2, 3] `shouldBe` False

    it "returns true" $ do
      isPalindrome "madamimadam" `shouldBe` True

    it "returns true" $ do
      isPalindrome [1 :: Int, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True

    it "returns true" $ do
      isPalindrome ([] :: [Char]) `shouldBe` True

  describe "Problem 7" $ do
    it "returns flat list (1)" $ do
      flatten (Elem (5 :: Int)) `shouldBe` [5]

    it "returns flat list (2)" $ do
      flatten (List [Elem (1 :: Int), List [Elem 2, List[Elem 3, Elem 4], Elem 5]]) `shouldBe` [1, 2, 3, 4, 5]

    it "returns empty list" $ do
      flatten (List [] :: NestedList Int) `shouldBe` []

  describe "Problem 8" $ do
    it "returns compressed list (1)" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

    it "returns compressed list (2)" $ do
      compress ([] :: [Char]) `shouldBe` []

    it "returns compressed list (3)" $ do
      compress [1 :: Int, 2, 3, 4] `shouldBe` [1, 2, 3, 4]
