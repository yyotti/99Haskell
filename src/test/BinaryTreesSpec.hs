module BinaryTreesSpec (main, spec) where

import Test.Hspec
import BinaryTrees

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Preparering" $ do
    it "returns (Branch 1 Empty Empty) if x = 1" $ do
      leaf (1 :: Int) `shouldBe` Branch 1 Empty Empty

  describe "Problem 55" $ do
    it "returns Empty if n = 0" $ do
      cbalTree 0 `shouldBe` [Empty]
    it "returns (B 'x' E E) if n = 1" $ do
      cbalTree 1 `shouldBe` [Branch 'x' Empty Empty]
    it "returns [(B 'x' (B 'x' E E) E), (B 'x' E (B 'x' E E))] if n = 2" $ do
      cbalTree 2 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) Empty, Branch 'x' Empty (Branch 'x' Empty Empty)]
    it "returns [(B 'x' (B 'x' E E) (B 'x' E (B 'x' E E))), (B 'x' (B 'x' E E) (B 'x' (B 'x' E E) E)), ...] if n = 4" $ do
      let expected = [
            Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty),
            Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
            Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),
            Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
            ] in
            cbalTree 4 `shouldBe` expected

  describe "Problem 56" $ do
    describe "mirror" $ do
      it "returns True if (t1 t2) = (E, E)" $ do
        mirror Empty Empty `shouldBe` True
      it "returns False if (t1 t2) = (E, (B 'x' E E))" $ do
        mirror Empty (Branch 'x' Empty Empty) `shouldBe` False
      it "returns True if (t1, t2) = ((B 'x' E E), (B 'x' E E))" $ do
        mirror (Branch 'x' Empty Empty) (Branch 'x' Empty Empty) `shouldBe` True
      it "returns False if (t1, t2) = ((B 'x' (B 'x' E E) E), (B 'x' E E))" $ do
        mirror (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty) `shouldBe` False
      it "returns False if (t1, t2) = ((B 'x' (B 'x' E E) E), (B 'x' (B 'x' E E) E))" $ do
        mirror (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
      it "returns True if (t1, t2) = ((B 'x' E (B 'x' E E)), (B 'x' (B 'x' E E) E))" $ do
        mirror (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` True
    describe "symmetric" $ do
      it "returns False if t = (B 'x' (B 'x' E E) E)" $ do
        symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
      it "returns True if t = (B 'x' (B 'x' E E) (B 'x' E E))" $ do
        symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` True
      it "returns False if t = (B 'x' (B 'x' E (B 'x' E E)) (B 'x' E (B 'x' E E)))" $ do
        symmetric (Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty (Branch 'x' Empty Empty))) `shouldBe` False
      it "returns True if t = (B 'x' (B 'x' (B 'x' E E) E) (B 'x' E (B 'x' E E)))" $ do
        symmetric (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))) `shouldBe` True

  describe "Problem 57" $ do
    it "returns Empty if ns = []" $ do
      construct [] `shouldBe` Empty
    it "returns (B 1 E E) if ns = [1]" $ do
      construct [1] `shouldBe` (Branch 1 Empty Empty)
    it "returns (B 3 (B 2 (B 1 E E) E) (B 5 E (B 7 E E))) if ls = [3, 2, 5, 7, 1]" $ do
      construct [3, 2, 5, 7, 1] `shouldBe` (Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty)))
    describe "symmetric test" $ do
      it "returns True if ns = [5, 3, 18, 1, 4, 12, 21]" $ do
        symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
      it "returns True if ns = [3, 2, 5, 7, 1]" $ do
        symmetric . construct $ [3, 2, 5, 7, 1]

  describe "Problem 58" $ do
    it "returns [E] if n = 0" $ do
      symCbalTrees 0 `shouldBe` [Empty]
    it "returns [(B 'x' E E)] if n = 1" $ do
      symCbalTrees 1 `shouldBe` [Branch 'x' Empty Empty]
    it "returns [] if n = 2" $ do
      symCbalTrees 2 `shouldBe` []
    it "returns [(B 'x' (B 'x' E E) (B 'x' E E))] if n = 3" $ do
      symCbalTrees 3 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]
    it "returns [(B 'x' (B 'x' E (B 'x' E E)) (B 'x' (B 'x' E E) E)), ...] if n = 5" $ do
      let expected = [
            Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
            Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty)
            ] in
            symCbalTrees 5 `shouldBe` expected

  describe "Problem 59" $ do
    it "returns [E] if (v, n) = ('x', 0)" $ do
      hbalTree 'x' 0 `shouldBe` [Empty]
    it "returns [(B 'x' E E)] if (v, n) = ('x', 1)" $ do
      hbalTree 'x' 1 `shouldBe` [Branch 'x' Empty Empty]
    it "returns [(B 'x' (B 'x' E E) E), (B 'x' E (B 'x' E E)), (B 'x' (B 'x' E E) (B 'x' E E))] if (v, n) = ('x', 2)" $ do
      hbalTree 'x' 2 `shouldBe` [(Branch 'x' (Branch 'x' Empty Empty) Empty), (Branch 'x' Empty (Branch 'x' Empty Empty)), (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))]
    it "returns 315 patterns if (v, n) = ('x', 3)" $ do
      (length $ hbalTree 'x' 4) `shouldBe` 315
