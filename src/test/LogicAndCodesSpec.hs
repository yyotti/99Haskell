module LogicAndCodesSpec (main, spec) where

import Test.Hspec
import LogicAndCodes

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problem 46" $ do
    describe "not'" $ do
      it "returns False when a = True" $ do
        not' True `shouldBe` False
      it "returns True when a = False" $ do
        not' False `shouldBe` True
    describe "and'" $ do
      it "returns True when (a, b) = (True, True)" $ do
        and' True True `shouldBe` True
      it "returns False when (a, b) = (True, False)" $ do
        and' True False `shouldBe` False
      it "returns False when (a, b) = (False, True)" $ do
        and' False True `shouldBe` False
      it "returns False when (a, b) = (False, False)" $ do
        and' False False `shouldBe` False
    describe "or'" $ do
      it "returns True when (a, b) = (True, True)" $ do
        or' True True `shouldBe` True
      it "returns True when (a, b) = (True, False)" $ do
        or' True False `shouldBe` True
      it "returns True when (a, b) = (False, True)" $ do
        or' False True `shouldBe` True
      it "returns False when (a, b) = (False, False)" $ do
        or' False False `shouldBe` False
    describe "nand'" $ do
      it "returns False when (a, b) = (True, True)" $ do
        nand' True True `shouldBe` False
      it "returns True when (a, b) = (True, False)" $ do
        nand' True False `shouldBe` True
      it "returns True when (a, b) = (False, True)" $ do
        nand' False True `shouldBe` True
      it "returns True when (a, b) = (False, False)" $ do
        nand' False False `shouldBe` True
    describe "nor'" $ do
      it "returns False when (a, b) = (True, True)" $ do
        nor' True True `shouldBe` False
      it "returns False when (a, b) = (True, False)" $ do
        nor' True False `shouldBe` False
      it "returns False when (a, b) = (False, True)" $ do
        nor' False True `shouldBe` False
      it "returns True when (a, b) = (False, False)" $ do
        nor' False False `shouldBe` True
    describe "xor'" $ do
      it "returns False when (a, b) = (True, True)" $ do
        xor' True True `shouldBe` False
      it "returns True when (a, b) = (True, False)" $ do
        xor' True False `shouldBe` True
      it "returns True when (a, b) = (False, True)" $ do
        xor' False True `shouldBe` True
      it "returns False when (a, b) = (False, False)" $ do
        xor' False False `shouldBe` False
    describe "impl'" $ do
      it "returns True when (a, b) = (True, True)" $ do
        impl' True True `shouldBe` True
      it "returns False when (a, b) = (True, False)" $ do
        impl' True False `shouldBe` False
      it "returns True when (a, b) = (False, True)" $ do
        impl' False True `shouldBe` True
      it "returns True when (a, b) = (False, False)" $ do
        impl' False False `shouldBe` True
    describe "equ'" $ do
      it "returns True when (a, b) = (True, True)" $ do
        equ' True True `shouldBe` True
      it "returns False when (a, b) = (True, False)" $ do
        equ' True False `shouldBe` False
      it "returns False when (a, b) = (False, True)" $ do
        equ' False True `shouldBe` False
      it "returns True when (a, b) = (False, False)" $ do
        equ' False False `shouldBe` True
    describe "table" $ do
      it "returns string when f = `and' a (or' a b)`" $ do
        table (\a b -> and' a (or' a b)) `shouldBe` "True True True\nTrue False True\nFalse True False\nFalse False False"
