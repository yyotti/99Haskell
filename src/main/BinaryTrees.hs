module BinaryTrees where
  import Data.List

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
  construct ls = foldl' addValue Empty ls
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

  {-
  - 7 Problem 59
  - (**) Construct height-balanced binary trees
  -
  - In a height-balanced binary tree, the following property holds for every node:
  - The height of its left subtree and the height of its right subtree are almost equal,
  - which means their difference is not greater than one.
  -
  - Construct a list of all height-balanced binary trees with the given element and the given maximum height.
  -
  - Example in Haskell:
  - *Main> take 4 $ hbalTree 'x' 3
  - [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
  -  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
  -  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
  -  Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
  -}
  hbalTree :: a -> Int -> [Tree a]
  hbalTree v n | n < 1 = [Empty]
               | n == 1 = [leaf v]
               | otherwise = ls2 ++ ls1
    where sub1 = hbalTree v (n - 1)
          sub2 = hbalTree v (n - 2)
          ls1 = concatMap (\t1 -> concatMap (\t2 -> [Branch v t1 t2]) sub1) sub1
          ls2 = concatMap (\t1 -> concatMap (\t2 -> [Branch v t1 t2, Branch v t2 t1]) sub2) sub1

  {-
  - 8 Problem 60
  - (**) Construct height-balanced binary trees with a given number of nodes
  -
  - Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
  -
  - Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult.
  - Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes
  - in a height-balanced binary tree of height H.
  - On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have?
  - Write a function maxHeight that computes this.
  -
  - Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes.
  - Find out how many height-balanced trees exist for N = 15.
  -
  - Example in Haskell:
  - *Main> length $ hbalTreeNodes 'x' 15
  - 1553
  - *Main> map (hbalTreeNodes 'x') [0..3]
  - [[Empty],
  -  [Branch 'x' Empty Empty],
  -  [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
  -  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
  -}
  minHbalNodes :: Int -> Int
  minHbalNodes h | h < 1 = 0
                 | otherwise = minHbalNodes (h - 1) + minHbalNodes (h - 2) + 1

  maxHbalHeight :: Int -> Int
  maxHbalHeight n | n < 1 = 0
                  | otherwise = last $ takeWhile (\k -> minHbalNodes k <= n) [1..]

  minHbalHeight :: Int -> Int
  minHbalHeight n | n < 1 = 0
                  | otherwise = minHbalHeight (n `div` 2) + 1

  nodeCount :: Tree a -> Int
  nodeCount Empty = 0
  nodeCount (Branch _ l r) = nodeCount l + nodeCount r + 1

  hbalTreeNodes :: a -> Int -> [Tree a]
  hbalTreeNodes v n | n < 1 = [Empty]
                    | n == 1 = [leaf v]
                    | otherwise = filter (\t -> nodeCount t == n) $ concatMap (\k -> hbalTree v k) [m1 .. m2]
    where m1 = minHbalHeight n
          m2 = maxHbalHeight n

  {-
  - 2 Problem 61
  - Count the leaves of a binary tree
  -
  - A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
  -
  - Example in Haskell:
  - > countLeaves tree4
  - 2
  -
  - Note: tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)
  -}
  countLeaves :: Tree a -> Int
  countLeaves Empty = 0
  countLeaves (Branch _ Empty Empty) = 1
  countLeaves (Branch _ l r) = countLeaves l + countLeaves r

  {-
  - 3 Problem 61A
  - Collect the leaves of a binary tree in a list
  -
  - A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.
  -
  - Example in Haskell:
  - > leaves tree4
  - [4,2]
  -}
  leaves :: Tree a -> [a]
  leaves Empty = []
  leaves (Branch v Empty Empty) = [v]
  leaves (Branch _ l r) = leaves l ++ leaves r

  {-
  - 4 Problem 62
  - Collect the internal nodes of a binary tree in a list
  -
  - An internal node of a binary tree has either one or two non-empty successors.
  - Write a predicate internals/2 to collect them in a list.
  -
  - Example in Haskell:
  - Prelude>internals tree4
  - Prelude>[1,2]
  -}
  internals :: Tree a -> [a]
  internals Empty = []
  internals (Branch _ Empty Empty) = []
  internals (Branch v l r) = v:internals l ++ internals r

  {-
  - 5 Problem 62B
  - Collect the nodes at a given level in a list
  -
  - A node of a binary tree is at level N if the path from the root to the node has length N-1.
  - The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.
  -
  - Example:
  -
  - % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
  - Example in Haskell:
  -
  - Prelude>atLevel tree4 2
  - Prelude>[2,2]
  -}
  atLevel :: Tree a -> Int -> [a]
  atLevel Empty _ = []
  atLevel (Branch v _ _) 1 = [v]
  atLevel (Branch _ l r) n = atLevel l (n - 1) ++ atLevel r (n - 1)

  {-
  - 6 Problem 63
  - Construct a complete binary tree
  -
  - A complete binary tree with height H is defined as follows:
  -
  - ・The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
  - ・In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted".
  -   This means that in a levelorder tree traversal all internal nodes come first, the leaves come second,
  -   and empty successors (the nil's which are not really nodes!) come last.
  -
  - Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
  -
  - We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order,
  - starting at the root with number 1. For every node X with address A the following property holds:
  - The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist.
  - This fact can be used to elegantly construct a complete binary tree structure.
  -
  - Write a predicate complete_binary_tree/2.
  -
  - Example in Haskell:
  - Main> completeBinaryTree 4
  - Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
  -
  - Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
  - True
  -}
  completeBinaryTree :: Int -> Tree Char
  completeBinaryTree n | n < 1 = Empty
                       | otherwise = constructTree 1
    where constructTree k | k > n = Empty
                          | otherwise = Branch 'x' (constructTree (2 * k)) (constructTree (2 * k + 1))

  isCompleteBinaryTree :: Tree a -> Bool
  isCompleteBinaryTree Empty = True
  isCompleteBinaryTree (Branch _ Empty Empty) = True
  isCompleteBinaryTree (Branch _ Empty _) = False
  isCompleteBinaryTree (Branch _ l r) = (isCompleteBinaryTree l) && (isCompleteBinaryTree r)
