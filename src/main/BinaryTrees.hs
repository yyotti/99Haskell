module BinaryTrees where
  data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
  leaf x = Branch x Empty Empty
