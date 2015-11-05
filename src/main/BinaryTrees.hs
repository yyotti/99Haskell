module BinaryTrees where
  data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

  leaf :: a -> Tree a
  leaf x = Branch x Empty Empty

  {-
  - 3 Problem 55
  - (**) Construct completely balanced binary trees
  -
  - In a completely balanced binary tree, the following property holds for every node:
  - The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
  - which means their difference is not greater than one.
  -
  - Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes.
  - The predicate should generate all solutions via backtracking.
  - Put the letter 'x' as information into all nodes of the tree.
  -
  - Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:
  -
  - *Main> cbalTree 4
  - [
  - -- permutation 1
  - --     x
  - --    / \
  - --   x   x
  - --        \
  - --         x
  - Branch 'x' (Branch 'x' Empty Empty)
  -            (Branch 'x' Empty
  -                                   (Branch 'x' Empty Empty)),
  -
  - -- permutation 2
  - --     x
  - --    / \
  - --   x   x
  - --      /
  - --     x
  - Branch 'x' (Branch 'x' Empty Empty)
  -            (Branch 'x' (Branch 'x' Empty Empty)
  -                        Empty),
  -
  - -- permutation 3
  - --     x
  - --    / \
  - --   x   x
  - --    \
  - --     x
  - Branch 'x' (Branch 'x' Empty
  -                        (Branch 'x' Empty Empty))
  -            (Branch 'x' Empty Empty),
  -
  - -- permutation 4
  - --     x
  - --    / \
  - --   x   x
  - --  /
  - -- x
  - Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
  -                        Empty)
  -            (Branch 'x' Empty Empty)
  - ]
  -}
  cbalTree :: Int -> [Tree Char]
  cbalTree n | n < 1 = [Empty]
             | odd n = concatMap (\t1 -> map (\t2 -> Branch 'x' t1 t2) t) t
             | otherwise = concatMap (\l -> concatMap (\r -> [Branch 'x' l r, Branch 'x' r l]) right) left
    where t = cbalTree $ (n - 1) `div` 2
          left = cbalTree $ n `div` 2
          right = cbalTree $ (n - (n `div` 2) - 1)

  {-
  - 4 Problem 56
  - (**) Symmetric binary trees
  -
  - Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree.
  - Write a predicate symmetric/1 to check whether a given binary tree is symmetric.
  - Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another.
  - We are only interested in the structure, not in the contents of the nodes.
  -
  - Example in Haskell:
  - *Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
  - False
  - *Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
  - True
  -}
  mirror :: Tree a -> Tree a -> Bool
  mirror Empty Empty = True
  mirror (Branch _ l1 r1) (Branch _ l2 r2) = (mirror l1 r2) && (mirror r1 l2)
  mirror _ _ = False

  symmetric :: Tree a -> Bool
  symmetric Empty = True
  symmetric (Branch _ l r) = mirror l r

  {-
  - 5 Problem 57
  - (**) Binary search trees (dictionaries)
  -
  - Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.
  -
  - Example in Haskell:
  - *Main> construct [3, 2, 5, 7, 1]
  - Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
  - *Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
  - True
  - *Main> symmetric . construct $ [3, 2, 5, 7, 1]
  - True
  -}
  construct :: [Int] -> Tree Int
  construct ls = foldl addValue Empty ls
    where addValue Empty x = leaf x
          addValue (Branch n l r) x | n > x = Branch n (addValue l x) r
                                    | otherwise = Branch n l (addValue r x)

  {-
  - 6 Problem 58
  - (**) Generate-and-test paradigm
  -
  - Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
  -
  - Example in Haskell:
  - *Main> symCbalTrees 5
  - [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),
  - Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
  -}
  symCbalTrees :: Int -> [Tree Char]
  symCbalTrees = filter symmetric . cbalTree
