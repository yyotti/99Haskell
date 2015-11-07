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

  {-
  - 7 Problem 64
  - Given a binary tree as the usual Prolog term t(X,L,R) (or nil).
  - As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node
  - in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below:
  -
  - (Image)
  -
  - In this layout strategy, the position of a node v is obtained by the following two rules:
  -
  - ・x(v) is equal to the position of the node v in the inorder sequence
  - ・y(v) is equal to the depth of the node v in the tree
  -
  - Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or
  - the rectangle bounding the drawn tree.
  -
  - Here is the example tree from the above illustration:
  -
  - tree64 = Branch 'n'
  -                 (Branch 'k'
  -                         (Branch 'c'
  -                                 (Branch 'a' Empty Empty)
  -                                 (Branch 'h'
  -                                         (Branch 'g'
  -                                                 (Branch 'e' Empty Empty)
  -                                                 Empty
  -                                         )
  -                                         Empty
  -                                 )
  -                         )
  -                         (Branch 'm' Empty Empty)
  -                 )
  -                 (Branch 'u'
  -                         (Branch 'p'
  -                                 Empty
  -                                 (Branch 's'
  -                                         (Branch 'q' Empty Empty)
  -                                         Empty
  -                                 )
  -                         )
  -                         Empty
  -                 )
  -
  - Example in Haskell:
  - > layout tree64
  - Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...
  -}
  layout :: Tree a -> Tree (a, (Int, Int))
  layout t = fst $ layout' t (1, 1)
    where layout' Empty (x, _) = (Empty, x)
          layout' (Branch v l r) (x, y) = (Branch (v, (x', y)) lt rt, nextX)
            where (lt, x') = layout' l (x, y + 1)
                  (rt, nextX) = layout' r (x' + 1, y + 1)

  {-
  - 8 Problem 65
  - An alternative layout method is depicted in the illustration below:
  -
  - (Image)
  -
  - Find out the rules and write the corresponding function.
  - Hint: On a given level, the horizontal distance between neighboring nodes is constant.
  -
  - Use the same conventions as in problem P64 and test your function in an appropriate way.
  -
  - Here is the example tree from the above illustration:
  -
  - tree65 = Branch 'n'
  -                 (Branch 'k'
  -                         (Branch 'c'
  -                                 (Branch 'a' Empty Empty)
  -                                 (Branch 'e'
  -                                         (Branch 'd' Empty Empty)
  -                                         (Branch 'g' Empty Empty)
  -                                 )
  -                         )
  -                         (Branch 'm' Empty Empty)
  -                 )
  -                 (Branch 'u'
  -                         (Branch 'p'
  -                                 Empty
  -                                 (Branch 'q' Empty Empty)
  -                         )
  -                         Empty
  -                 )
  - Example in Haskell:
  - > layout tree65
  - Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...
  -}
  layout2 :: Tree a -> Tree (a, (Int, Int))
  layout2 t = layout2' t x0 1 (d - 2)
    where x0 = (sum . map (\n -> 2^(d - n))) [2 .. leftmostNodeDepth t] + 1
          d = treeDepth t
          treeDepth Empty = 0
          treeDepth (Branch _ l r) = max (treeDepth l) (treeDepth r) + 1
          leftmostNodeDepth :: Tree a -> Int
          leftmostNodeDepth Empty = 0
          leftmostNodeDepth (Branch _ l _) = leftmostNodeDepth l + 1
          layout2' Empty _ _ _ = Empty
          layout2' (Branch v l r) x depth ex = Branch (v, (x, depth)) left right
            where left = layout2' l (x - 2^ex) (depth + 1) (ex - 1)
                  right = (layout2' r (x + 2^ex) (depth + 1) (ex - 1))

  {-
  - 9 Problem 66
  - Yet another layout strategy is shown in the illustration below:
  -
  - p66.gif
  -
  - The method yields a very compact layout while maintaining a certain symmetry in every node.
  - Find out the rules and write the corresponding Prolog predicate.
  - Hint: Consider the horizontal distance between a node and its successor nodes.
  - How tight can you pack together two subtrees to construct the combined binary tree?
  -
  - Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way.
  - Note: This is a difficult problem. Don't give up too early!
  -
  - Which layout do you like most?
  -
  - Example in Haskell:
  - > layout tree65
  - Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...
  -}
  layout3 :: Tree a -> Tree (a, (Int, Int))
  layout3 _ = undefined

  {-
  - 10 Problem 67A
  - A string representation of binary trees
  -
  - Somebody represents binary trees as strings of the following type:
  -
  - a(b(d,e),c(,f(g,)))
  -
  - a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term).
  -    Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form.
  -    Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.
  -
  - Example in Haskell:
  - Main> stringToTree "x(y,a(,b))" >>= print
  - Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
  - Main> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
  - True
  -}
  treeToString :: Tree Char -> String
  treeToString Empty = ""
  treeToString (Branch v Empty Empty) = [v]
  treeToString (Branch v l r) = v:"(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

  stringToTree :: String -> Tree Char
  stringToTree "" = Empty
  stringToTree [c] = leaf c
  stringToTree s = (snd . stringToTree') s
    where stringToTree' (c:cs) | c == ',' || c == ')' = (c:cs, Empty)
          stringToTree' (c1:c2:cs) | c2 == ',' || c2 == ')' = (c2:cs, leaf c1)
                                   | c2 == '(' = let
                                                      (',':cs', l) = stringToTree' cs
                                                      (')':cs'', r) = stringToTree' cs'
                                                    in (cs'', Branch c1 l r)
          stringToTree' _ = error "illegal format"

  {-
  - 11 Problem 68
  - Preorder and inorder sequences of binary trees.
  - We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.
  -
  - a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively.
  - The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.
  -
  - b) Can you use preorder/2 from problem part a) in the reverse direction;
  - i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.
  -
  - c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given,
  - then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.
  -
  - Example in Haskell:
  - Main> let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ;
  -             po = treeToPreorder t ;
  -             io = treeToInorder t } in preInTree po io >>= print
  - Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
  -}
  treeToPreorder :: Tree a -> [a]
  treeToPreorder Empty = []
  treeToPreorder (Branch v l r) = v:treeToPreorder l ++ treeToPreorder r

  treeToInorder :: Tree a -> [a]
  treeToInorder Empty = []
  treeToInorder (Branch v l r) = treeToInorder l ++ [v] ++ treeToInorder r

  preInTree :: Eq a => [a] -> [a] -> Tree a
  preInTree po io | length po /= length io = Empty
  preInTree (p:ps) io = Branch p (preInTree pl il) (preInTree pr (tail ir))
    where (il, ir) = span (/= p) io
          (pl, pr) = splitAt (length il) ps
  preInTree _ _ = Empty

  {-
  - 12 Problem 69
  - Dotstring representation of binary trees.
  -
  - We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.
  - Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted
  - where an empty subtree (nil) is encountered during the tree traversal.
  - For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'.
  - First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does
  - the conversion in both directions. Use difference lists.
  -
  - Example in Haskell:
  - > fst (ds2tree example)
  - Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
  -
  -  > tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
  -  "xy..z0..."
  -}
  ds2tree :: String -> (Tree Char, String)
  ds2tree [] = (Empty, "")
  ds2tree ('.':cs) = (Empty, cs)
  ds2tree (c:cs) = (Branch c l r, cs'')
    where (l, cs') = ds2tree cs
          (r, cs'') = ds2tree cs'

  tree2ds :: Tree Char -> String
  tree2ds Empty = "."
  tree2ds (Branch v l r) = v:tree2ds l ++ tree2ds r
