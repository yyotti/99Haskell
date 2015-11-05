module BinaryTreesSpec (main, spec) where

import Test.Hspec
import BinaryTrees

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Preparering" $ do
    it "returns (Branch 1 Empty Empty) when x = 1" $ do
      leaf (1 :: Int) `shouldBe` Branch 1 Empty Empty

  describe "Problem 55" $ do
    it "returns Empty when n = 0" $ do
      cbalTree 0 `shouldBe` [Empty]
    it "returns (B 'x' E E) when n = 1" $ do
      cbalTree 1 `shouldBe` [Branch 'x' Empty Empty]
    it "returns [(B 'x' (B 'x' E E) E), (B 'x' E (B 'x' E E))] when n = 2" $ do
      cbalTree 2 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) Empty, Branch 'x' Empty (Branch 'x' Empty Empty)]
    it "returns [(B 'x' (B 'x' E E) (B 'x' E (B 'x' E E))), (B 'x' (B 'x' E E) (B 'x' (B 'x' E E) E)), ...] when n = 4" $ do
      let expected = [
            Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty),
            Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
            Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),
            Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
            ] in
            cbalTree 4 `shouldBe` expected

  describe "Problem 56" $ do
    describe "mirror" $ do
      it "returns True when (t1 t2) = (E, E)" $ do
        mirror Empty Empty `shouldBe` True
      it "returns False when (t1 t2) = (E, (B 'x' E E))" $ do
        mirror Empty (Branch 'x' Empty Empty) `shouldBe` False
      it "returns True when (t1, t2) = ((B 'x' E E), (B 'x' E E))" $ do
        mirror (Branch 'x' Empty Empty) (Branch 'x' Empty Empty) `shouldBe` True
      it "returns False when (t1, t2) = ((B 'x' (B 'x' E E) E), (B 'x' E E))" $ do
        mirror (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty) `shouldBe` False
      it "returns False when (t1, t2) = ((B 'x' (B 'x' E E) E), (B 'x' (B 'x' E E) E))" $ do
        mirror (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
      it "returns True when (t1, t2) = ((B 'x' E (B 'x' E E)), (B 'x' (B 'x' E E) E))" $ do
        mirror (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` True
    describe "symmetric" $ do
      it "returns False when t = (B 'x' (B 'x' E E) E)" $ do
        symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
      it "returns True when t = (B 'x' (B 'x' E E) (B 'x' E E))" $ do
        symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` True
      it "returns False when t = (B 'x' (B 'x' E (B 'x' E E)) (B 'x' E (B 'x' E E)))" $ do
        symmetric (Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty (Branch 'x' Empty Empty))) `shouldBe` False
      it "returns True when t = (B 'x' (B 'x' (B 'x' E E) E) (B 'x' E (B 'x' E E)))" $ do
        symmetric (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))) `shouldBe` True

  describe "Problem 57" $ do
    it "returns Empty when ns = []" $ do
      construct [] `shouldBe` Empty
    it "returns (B 1 E E) when ns = [1]" $ do
      construct [1] `shouldBe` (Branch 1 Empty Empty)
    it "returns (B 3 (B 2 (B 1 E E) E) (B 5 E (B 7 E E))) when ls = [3, 2, 5, 7, 1]" $ do
      construct [3, 2, 5, 7, 1] `shouldBe` (Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty)))
    describe "symmetric test" $ do
      it "returns True when ns = [5, 3, 18, 1, 4, 12, 21]" $ do
        symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
      it "returns True when ns = [3, 2, 5, 7, 1]" $ do
        symmetric . construct $ [3, 2, 5, 7, 1]

  describe "Problem 58" $ do
    it "returns [E] when n = 0" $ do
      symCbalTrees 0 `shouldBe` [Empty]
    it "returns [(B 'x' E E)] when n = 1" $ do
      symCbalTrees 1 `shouldBe` [Branch 'x' Empty Empty]
    it "returns [] when n = 2" $ do
      symCbalTrees 2 `shouldBe` []
    it "returns [(B 'x' (B 'x' E E) (B 'x' E E))] when n = 3" $ do
      symCbalTrees 3 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]
    it "returns [(B 'x' (B 'x' E (B 'x' E E)) (B 'x' (B 'x' E E) E)), ...] when n = 5" $ do
      let expected = [
            Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
            Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty)
            ] in
            symCbalTrees 5 `shouldBe` expected

  describe "Problem 59" $ do
    it "returns [E] when (v, n) = ('x', 0)" $ do
      hbalTree 'x' 0 `shouldBe` [Empty]
    it "returns [(B 'x' E E)] when (v, n) = ('x', 1)" $ do
      hbalTree 'x' 1 `shouldBe` [Branch 'x' Empty Empty]
    it "returns [(B 'x' (B 'x' E E) E), (B 'x' E (B 'x' E E)), (B 'x' (B 'x' E E) (B 'x' E E))] when (v, n) = ('x', 2)" $ do
      hbalTree 'x' 2 `shouldBe` [(Branch 'x' (Branch 'x' Empty Empty) Empty), (Branch 'x' Empty (Branch 'x' Empty Empty)), (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))]
    it "returns 315 patterns when (v, n) = ('x', 3)" $ do
      (length $ hbalTree 'x' 4) `shouldBe` 315

  describe "Problem 60" $ do
    describe "minHbalNodes" $ do
      it "returns 0 when h = 0" $ do
        minHbalNodes 0 `shouldBe` 0
      it "returns 1 when h = 1" $ do
        minHbalNodes 1 `shouldBe` 1
      it "returns 2 when h = 2" $ do
        minHbalNodes 2 `shouldBe` 2
      it "returns 4 when h = 3" $ do
        minHbalNodes 3 `shouldBe` 4
      it "returns 7 when h = 4" $ do
        minHbalNodes 4 `shouldBe` 7
      it "returns 12 when h = 5" $ do
        minHbalNodes 5 `shouldBe` 12
    describe "maxHbalHeight" $ do
      it "returns 0 when h = 0" $ do
        maxHbalHeight 0 `shouldBe` 0
      it "returns 1 when h = 1" $ do
        maxHbalHeight 1 `shouldBe` 1
      it "returns 2 when h = 2" $ do
        maxHbalHeight 2 `shouldBe` 2
      it "returns 2 when h = 3" $ do
        maxHbalHeight 3 `shouldBe` 2
      it "returns 3 when h = 4" $ do
        maxHbalHeight 4 `shouldBe` 3
      it "returns 3 when h = 5" $ do
        maxHbalHeight 5 `shouldBe` 3
      it "returns 4 when h = 7" $ do
        maxHbalHeight 7 `shouldBe` 4
      it "returns 4 when h = 8" $ do
        maxHbalHeight 8 `shouldBe` 4
    describe "minHbalHeight" $ do
      it "returns 0 when h = 0" $ do
        minHbalHeight 0 `shouldBe` 0
      it "returns 1 when h = 1" $ do
        minHbalHeight 1 `shouldBe` 1
      it "returns 2 when h = 2" $ do
        minHbalHeight 2 `shouldBe` 2
      it "returns 2 when h = 3" $ do
        minHbalHeight 3 `shouldBe` 2
      it "returns 3 when h = 4" $ do
        minHbalHeight 4 `shouldBe` 3
      it "returns 3 when h = 5" $ do
        minHbalHeight 5 `shouldBe` 3
      it "returns 4 when h = 7" $ do
        minHbalHeight 7 `shouldBe` 3
      it "returns 4 when h = 8" $ do
        minHbalHeight 8 `shouldBe` 4
    describe "nodeCount" $ do
      it "returns 0 when t = E" $ do
        nodeCount Empty `shouldBe` 0
      it "returns 1 when t = (B 'x' E E)" $ do
        nodeCount (Branch 'x' Empty Empty) `shouldBe` 1
      it "returns 2 when t = (B 'x' (B 'x' E E) E)" $ do
        nodeCount (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` 2
      it "returns 2 when t = (B 'x' E (B 'x' E E))" $ do
        nodeCount (Branch 'x' Empty (Branch 'x' Empty Empty)) `shouldBe` 2
      it "returns 3 when t = (B 'x' (B 'x' E E) (B 'x' E E))" $ do
        nodeCount (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` 3
      it "returns 4 when t = (B 'x' (B 'x' (B 'x' E E) E) (B 'x' E E))" $ do
        nodeCount (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)) `shouldBe` 4
      it "returns 4 when t = (B 'x' (B 'x' E (B 'x' E E)) (B 'x' E E))" $ do
        nodeCount (Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)) `shouldBe` 4
      it "returns 4 when t = (B 'x' (B 'x' E E) (B 'x' (B 'x' E E) E))" $ do
        nodeCount (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty)) `shouldBe` 4
      it "returns 4 when t = (B 'x' (B 'x' E E) (B 'x' E (B 'x' E E)))" $ do
        nodeCount (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))) `shouldBe` 4
      it "returns 5 when t = (B 'x' (B 'x' (B 'x' E E) (B 'x' E E)) (B 'x' E E))" $ do
        nodeCount (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)) `shouldBe` 5
      it "returns 5 when t = (B 'x' (B 'x' (B 'x' E E) E) (B 'x' (B 'x' E E) E))" $ do
        nodeCount (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty)) `shouldBe` 5
    describe "hbalTreeNodes" $ do
      it "returns [E] when (v, n) = ('x', 0)" $ do
        hbalTreeNodes 'x' 0 `shouldBe` [Empty]
      it "returns [(B 'x' E E)] when (v, n) = ('x', 1)" $ do
        hbalTreeNodes 'x' 1 `shouldBe` [Branch 'x' Empty Empty]
      it "returns [(B 'x' (B 'x' E E) E), (B 'x' E (B 'x' E E))] when (v, n) = ('x', 2)" $ do
        hbalTreeNodes 'x' 2 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) Empty, Branch 'x' Empty (Branch 'x' Empty Empty)]
      it "returns [(B 'x' (B 'x' E E) (B 'x' E E))] when (v, n) = ('x', 3)" $ do
        hbalTreeNodes 'x' 3 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]
      it "returns [(B 'x' (B 'x' (B 'x' E E) E) (B 'x' E E)), ...] when (v, n) = ('x', 4)" $ do
        let expected = [
              Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty),
              Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
              Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),
              Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
              ] in
              hbalTreeNodes 'x' 4 `shouldBe` expected
      -- FIXME ちょっと時間かかるので少し消しておく
      -- it "returns 1553 patterns when (v, n) = ('x', 15)" $ do
      --   length (hbalTreeNodes 'x' 15) `shouldBe` 1553

  describe "Problem 61" $ do
    it "returns 0 when t = E" $ do
      countLeaves Empty `shouldBe` 0
    it "returns 1 when t = (B 'x' E E)" $ do
      countLeaves (Branch 'x' Empty Empty) `shouldBe` 1
    it "returns 1 when t = (B 'x' (B 'x' E E) E)" $ do
      countLeaves (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` 1
    it "returns 1 when t = (B 'x' E (B 'x' E E))" $ do
      countLeaves (Branch 'x' Empty (Branch 'x' Empty Empty)) `shouldBe` 1
    it "returns 2 when t = (B 'x' (B 'x' E E) (B 'x' E E))" $ do
      countLeaves (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` 2
    it "returns 2 when t = (B 'x' (B 'x' (B 'x' E E) E) (B 'x' E E))" $ do
      countLeaves (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)) `shouldBe` 2
    it "returns 2 when t = (B 'x' (B 'x' E (B 'x' E E)) (B 'x' E E))" $ do
      countLeaves (Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)) `shouldBe` 2
    it "returns 2 when t = (B 'x' (B 'x' E E) (B 'x' (B 'x' E E) E))" $ do
      countLeaves (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty)) `shouldBe` 2
    it "returns 2 when t = (B 'x' (B 'x' E E) (B 'x' E (B 'x' E E)))" $ do
      countLeaves (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))) `shouldBe` 2
    it "returns 2 when t = (B 1 (B 2 E (B 4 E E)) (B 2 E E))" $ do
      countLeaves (Branch (1 :: Int) (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) `shouldBe` 2
    it "returns 3 when t = (B 'x' (B 'x' (B 'x' E E) (B 'x' E E)) (B 'x' E E))" $ do
      countLeaves (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)) `shouldBe` 3
    it "returns 2 when t = (B 'x' (B 'x' (B 'x' E E) E) (B 'x' (B 'x' E E) E))" $ do
      countLeaves (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty)) `shouldBe` 2

  describe "Problem 61A" $ do
    it "returns [] when t = E" $ do
      leaves Empty `shouldBe` ([] :: [Int])
    it "returns [1] when t = (B 1 E E)" $ do
      leaves (Branch (1 :: Int) Empty Empty) `shouldBe` [1]
    it "returns [2] when t = (B 1 (B 2 E E) E)" $ do
      leaves (Branch (1 :: Int) (Branch 2 Empty Empty) Empty) `shouldBe` [2]
    it "returns [3] when t = (B 1 E (B 3 E E))" $ do
      leaves (Branch (1 :: Int) Empty (Branch 3 Empty Empty)) `shouldBe` [3]
    it "returns [2, 3] when t = (B 1 (B 2 E E) (B 3 E E))" $ do
      leaves (Branch (1 :: Int) (Branch 2 Empty Empty) (Branch 3 Empty Empty)) `shouldBe` [2, 3]
    it "returns [3, 4] when t = (B 1 (B 2 (B 3 E E) E) (B 4 E E))" $ do
      leaves (Branch (1 :: Int) (Branch 2 (Branch 3 Empty Empty) Empty) (Branch 4 Empty Empty)) `shouldBe` [3, 4]
    it "returns [3, 4] when t = (B 1 (B 2 E (B 3 E E)) (B 4 E E))" $ do
      leaves (Branch (1 :: Int) (Branch 2 Empty (Branch 3 Empty Empty)) (Branch 4 Empty Empty)) `shouldBe` [3, 4]
    it "returns [2, 4] when t = (B 1 (B 2 E E) (B 3 (B 4 E E) E))" $ do
      leaves (Branch (1 :: Int) (Branch 2 Empty Empty) (Branch 3 (Branch 4 Empty Empty) Empty)) `shouldBe` [2, 4]
    it "returns [2, 4] when t = (B 1 (B 2 E E) (B 3 E (B 4 E E)))" $ do
      leaves (Branch (1 :: Int) (Branch 2 Empty Empty) (Branch 3 Empty (Branch 4 Empty Empty))) `shouldBe` [2, 4]
    it "returns [4, 2] when t = (B 1 (B 2 E (B 4 E E)) (B 2 E E))" $ do
      leaves (Branch (1 :: Int) (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) `shouldBe` [4, 2]
    it "returns [3, 4, 5] when t = (B 1 (B 2 (B 3 E E) (B 4 E E)) (B 5 E E))" $ do
      leaves (Branch (1 :: Int) (Branch 2 (Branch 3 Empty Empty) (Branch 4 Empty Empty)) (Branch 5 Empty Empty)) `shouldBe` [3, 4, 5]
    it "returns [3, 5] when t = (B 1 (B 2 (B 3 E E) E) (B 4 (B 5 E E) E))" $ do
      leaves (Branch (1 :: Int) (Branch 2 (Branch 3 Empty Empty) Empty) (Branch 4 (Branch 5 Empty Empty) Empty)) `shouldBe` [3, 5]

  describe "Problem 62" $ do
    it "returns [] when t = E" $ do
      internals Empty `shouldBe` ([] :: [Int])
    it "returns [] when t = (B 1 E E)" $ do
      internals (Branch (1 :: Int) Empty Empty) `shouldBe` []
    it "returns [1] when t = (B 1 (B 2 E E) E)" $ do
      internals (Branch (1 :: Int) (Branch 2 Empty Empty) Empty) `shouldBe` [1]
    it "returns [1] when t = (B 1 E (B 3 E E))" $ do
      internals (Branch (1 :: Int) Empty (Branch 3 Empty Empty)) `shouldBe` [1]
    it "returns [1] when t = (B 1 (B 2 E E) (B 3 E E))" $ do
      internals (Branch (1 :: Int) (Branch 2 Empty Empty) (Branch 3 Empty Empty)) `shouldBe` [1]
    it "returns [1, 2] when t = (B 1 (B 2 (B 3 E E) E) (B 4 E E))" $ do
      internals (Branch (1 :: Int) (Branch 2 (Branch 3 Empty Empty) Empty) (Branch 4 Empty Empty)) `shouldBe` [1, 2]
    it "returns [1, 2] when t = (B 1 (B 2 E (B 3 E E)) (B 4 E E))" $ do
      internals (Branch (1 :: Int) (Branch 2 Empty (Branch 3 Empty Empty)) (Branch 4 Empty Empty)) `shouldBe` [1, 2]
    it "returns [1, 3] when t = (B 1 (B 2 E E) (B 3 (B 4 E E) E))" $ do
      internals (Branch (1 :: Int) (Branch 2 Empty Empty) (Branch 3 (Branch 4 Empty Empty) Empty)) `shouldBe` [1, 3]
    it "returns [1, 3] when t = (B 1 (B 2 E E) (B 3 E (B 4 E E)))" $ do
      internals (Branch (1 :: Int) (Branch 2 Empty Empty) (Branch 3 Empty (Branch 4 Empty Empty))) `shouldBe` [1, 3]
    it "returns [1, 2] when t = (B 1 (B 2 E (B 4 E E)) (B 2 E E))" $ do
      internals (Branch (1 :: Int) (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) `shouldBe` [1, 2]
    it "returns [1, 2] when t = (B 1 (B 2 (B 3 E E) (B 4 E E)) (B 5 E E))" $ do
      internals (Branch (1 :: Int) (Branch 2 (Branch 3 Empty Empty) (Branch 4 Empty Empty)) (Branch 5 Empty Empty)) `shouldBe` [1, 2]
    it "returns [1, 2, 4] when t = (B 1 (B 2 (B 3 E E) E) (B 4 (B 5 E E) E))" $ do
      internals (Branch (1 :: Int) (Branch 2 (Branch 3 Empty Empty) Empty) (Branch 4 (Branch 5 Empty Empty) Empty)) `shouldBe` [1, 2, 4]
