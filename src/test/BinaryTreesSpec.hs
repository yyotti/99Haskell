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
