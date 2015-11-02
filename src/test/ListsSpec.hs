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

    it "returns the kth element of a list (2)" $ do
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
      compress [1 :: Int, 2, 3, 4] `shouldBe` [1, 2, 3, 4]

    it "returns empty list" $ do
      compress ([] :: [Char]) `shouldBe` []

  describe "Problem 9" $ do
    it "returns packed list (1)" $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]

    it "returns packed list (2)" $ do
      pack [1 :: Int, 2, 3, 4] `shouldBe` [[1], [2], [3], [4]]

    it "returns empty list" $ do
      pack ([] :: [Int]) `shouldBe` []

  describe "Problem 10" $ do
    it "returns encoded list (1)" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]

    it "returns encoded list (2)" $ do
      encode [1 :: Int, 2, 3, 4] `shouldBe` [(1, 1), (1, 2), (1, 3), (1, 4)]

    it "returns empty list" $ do
      encode ([] :: [Int]) `shouldBe` []

  describe "Problem 11" $ do
    it "returns run-length encoded list (1)" $ do
      encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

    it "returns run-length encoded list (2)" $ do
      encodeModified [1 :: Int, 2, 3, 4] `shouldBe` [Single 1, Single 2, Single 3, Single 4]

    it "returns empty list" $ do
      encodeModified ([] :: [Int]) `shouldBe` []

  describe "Problem 12" $ do
    it "returns decoded list (1)" $ do
      decodeModified [Multiple (4 :: Int) 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"

    it "returns decoded list (2)" $ do
      decodeModified [Single (1 :: Int), Single 2, Single 3, Single 4] `shouldBe` [1, 2, 3, 4]

    it "returns empty list" $ do
      decodeModified ([] :: [Code Int]) `shouldBe` []

  describe "Problem 13" $ do
    it "returns run-length encoded list (direct) (1)" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Multiple 1 'b', Multiple 2 'c', Multiple 2 'a', Multiple 1 'd', Multiple 4 'e']

    it "returns run-length encoded list (direct) (2)" $ do
      encodeDirect [1 :: Int, 2, 3, 4] `shouldBe` [Multiple 1 1, Multiple 1 2, Multiple 1 3, Multiple 1 4]

    it "returns empty list" $ do
      encodeDirect ([] :: [Int]) `shouldBe` []

  describe "Problem 14" $ do
    it "returns duplicated list (1)" $ do
      dupli [1 :: Int, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]

    it "returns duplicated list (2)" $ do
      dupli "abccd" `shouldBe` "aabbccccdd"

    it "returns empty list" $ do
      dupli ([] :: [Char]) `shouldBe` []

  describe "Problem 15" $ do
    it "returns replicated list (1)" $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"

    it "returns replicated list (2)" $ do
      repli [1 :: Int, 2, 3] 2 `shouldBe` [1, 1, 2, 2, 3, 3]

    it "returns empty list (1)" $ do
      repli ([] :: [Char]) 2 `shouldBe` []

    it "returns empty list (2)" $ do
      repli [1 :: Int, 2, 3] 0 `shouldBe` []

    it "throws an exception if n was negative" $ do
      evaluate (repli [1 :: Int, 2, 3] (-1)) `shouldThrow` errorCall "illegal argument n:-1"

  describe "Problem 16" $ do
    it "returns dropped list (1)" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

    it "returns dropped list (2)" $ do
      dropEvery [1 :: Int, 2, 3] 1 `shouldBe` []

    it "returns empty list" $ do
      dropEvery ([] :: [Int]) 2 `shouldBe` []

    it "throws an exception if n was less or equals 0" $ do
      evaluate (dropEvery ['a', 'b', 'c'] 0) `shouldThrow` errorCall "illegal argument n:0"

  describe "Problem 17" $ do
    it "returns splitted list pair (1)" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

    it "returns splitted list pair (2)" $ do
      split [1 :: Int, 2, 3] 0 `shouldBe` ([], [1, 2, 3])

    it "returns splitted list pair (3)" $ do
      split [1 :: Int, 2, 3] 10 `shouldBe` ([1, 2, 3], [])

    it "returns empty list pair" $ do
      split ([] :: [Int]) 2 `shouldBe` ([], [])

    it "throws an exception if n was negative" $ do
      evaluate (split ['a', 'b', 'c'] (-1)) `shouldThrow` errorCall "illegal argument n:-1"

  describe "Problem 18" $ do
    it "returns sliced list (1)" $ do
      slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7 `shouldBe` "cdefg"

    it "returns sliced list (2)" $ do
      slice [1 :: Int, 2, 3] 0 8 `shouldBe` [1, 2, 3]

    it "returns empty list (1)" $ do
      slice ([] :: [Int]) 2 5 `shouldBe` []

    it "returns empty list (2)" $ do
      slice [1 :: Int, 2, 3] 3 2 `shouldBe` []

  describe "Problem 19" $ do
    it "returns rotated list (1)" $ do
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3 `shouldBe` "defghabc"

    it "returns rotated list (2)" $ do
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2) `shouldBe` "ghabcdef"

    it "returns sliced list (3)" $ do
      rotate [1 :: Int, 2, 3, 4, 5] 9 `shouldBe` [5, 1, 2, 3, 4]

    it "returns sliced list (3)" $ do
      rotate [1 :: Int, 2, 3, 4, 5] (-22) `shouldBe` [4, 5, 1, 2, 3]

    it "returns empty list" $ do
      rotate ([] :: [Int]) 2 `shouldBe` []

  describe "Problem 20" $ do
    it "returns removed list (1)" $ do
      removeAt 2 "abcd" `shouldBe` ('b', "acd")

    it "throws an exception if n was less or equals 0" $ do
      evaluate (removeAt (-1) ['a', 'b', 'c']) `shouldThrow` errorCall "illegal argument n:-1"

    it "throws an exception if list was empty" $ do
      evaluate (removeAt 2 []) `shouldThrow` errorCall "list is empty"

    it "throws an exception if n was grater than length of list" $ do
      evaluate (removeAt 4 [1 :: Int, 2, 3]) `shouldThrow` errorCall "illegal argument n:4"

  describe "Problem 21" $ do
    it "returns inserted list (1)" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    it "returns inserted list (1)" $ do
      insertAt 2 [1 :: Int, 2, 3, 4] 1 `shouldBe` [2, 1, 2, 3, 4]

  describe "Problem 22" $ do
    it "returns list contains 4 to 9" $ do
      range 4 9 `shouldBe` [4, 5, 6, 7, 8, 9]

    it "returns list contains only 2" $ do
      range 2 2 `shouldBe` [2]

    it "returns empty list" $ do
      range 4 1 `shouldBe` []

  -- TODO ランダムのテストのしかたがよく分からん
  describe "Problem 23" $ do
    it "returns empty list" $ do
      rnd_select [1 :: Int, 2, 3] 0 `shouldReturn` []

    it "returns list" $ do
      rnd_select [1 :: Int] 4 `shouldReturn` [1, 1, 1, 1]

    it "throws an exception if n was negative" $ do
      evaluate (rnd_select ['a', 'b', 'c'] (-1)) `shouldThrow` errorCall "illegal argument n:-1"

    it "throws an exception if list was empty" $ do
      evaluate (rnd_select [] 2) `shouldThrow` errorCall "list is empty"

  -- TODO ランダムのテストのしかたがよく分からん
  describe "Problem 24" $ do
    it "returns list" $ do
      diff_select 6 1 `shouldReturn` [1, 1, 1, 1, 1, 1]

    it "throws an exception if n was negative" $ do
      evaluate (diff_select (-1) 49) `shouldThrow` errorCall "illegal argument n:-1"

    it "throws an exception if m was 0 or negative" $ do
      evaluate (diff_select 6 (-1)) `shouldThrow` errorCall "illegal argument m:-1"

  -- TODO ランダムのテストのしかたがよく分からん
  describe "Problem 25" $ do
    it "returns list" $ do
      rnd_permu "a" `shouldReturn` ['a']

    it "returns list" $ do
      rnd_permu [1 :: Int, 1, 1] `shouldReturn` [1, 1, 1]

    it "returns empty list" $ do
      rnd_permu ([] :: [Int]) `shouldReturn` []
