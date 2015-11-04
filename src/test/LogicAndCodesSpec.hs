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

  describe "Problem 47" $ do
    it "returns string when f = (a `and'` (a `or'` not' b))" $ do
      table (\a b -> a `and'` (a `or'` not' b)) `shouldBe` "True True True\nTrue False True\nFalse True False\nFalse False False"

  describe "Problem 48" $ do
    it "returns string when f = ((a `and'` (b `or'` c)) `equ'` ((a `and'` b) `or'` (a `and'` c)))" $ do
      -- infixl 3 `equ'
      let str1 = (
            "True  True  True  True \n" ++
            "True  True  False True \n" ++
            "True  False True  True \n" ++
            "True  False False True \n" ++
            "False True  True  True \n" ++
            "False True  False True \n" ++
            "False False True  True \n" ++
            "False False False True "
                 )
          in tablen 3 (\[a, b, c] -> (a `and'` (b `or'` c)) `equ'` ((a `and'` b) `or'` (a `and'` c))) `shouldBe` str1
    it "returns string when f = ((a `and'` (b `or'` c)) `equ'` ((a `and'` b) `or'` (a `and'` c)))" $ do
      -- infixl 7 `equ'
      let str2 = (
            "True  True  True  True \n" ++
            "True  True  False True \n" ++
            "True  False True  True \n" ++
            "True  False False False\n" ++
            "False True  True  False\n" ++
            "False True  False False\n" ++
            "False False True  False\n" ++
            "False False False False"
                 )
          in tablen 3 (\[a, b, c] -> (a `and'` ((b `or'` c) `equ'` a) `and'` b) `or'` (a `and'` c)) `shouldBe` str2

  describe "Problem 49" $ do
    it "returns 1-bit gray code" $ do
      gray 1 `shouldBe` ["0", "1"]
    it "returns 2-bit gray code" $ do
      gray 2 `shouldBe` ["00", "01", "11", "10"]
    it "returns 3-bit gray code" $ do
      gray 3 `shouldBe` ["000","001","011","010","110","111","101","100"]
