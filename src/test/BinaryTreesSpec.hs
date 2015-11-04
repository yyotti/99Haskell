module BinaryTreesSpec (main, spec) where

import Test.Hspec
import BinaryTrees
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Preparering" $ do
    it "returns (Branch 1 Empty Empty) if x = 1" $ do
      leaf 1 `shouldBe` Branch 1 Empty Empty
