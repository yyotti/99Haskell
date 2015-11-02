module Lists where
  import System.Random (getStdRandom, randomR)
  import Control.Monad
  import Data.List (sortBy)

  {-
  - 1 Problem 1
  - (*) Find the last element of a list.
  -
  - Example in Haskell:
  - Prelude> myLast [1,2,3,4]
  - 4
  - Prelude> myLast ['x','y','z']
  - 'z'
  -}
  myLast :: [a] -> a
  myLast [] = error "list is empty"
  myLast [x] = x
  myLast (_:xs) = myLast xs

  {-
  - 2 Problem 2
  - (*) Find the last but one element of a list.
  -
  - Example in Haskell:
  - Prelude> myButLast [1,2,3,4]
  - 3
  - Prelude> myButLast ['a'..'z']
  - 'y'
  -}
  myButLast :: [a] -> a
  myButLast [] = error "list is empty"
  myButLast [_] = error "list has only one element"
  myButLast [x, _] = x
  myButLast (_:xs) = myButLast xs

  {-
  - 3 Problem 3
  - (*) Find the K'th element of a list. The first element in the list is number 1.
  -
  - Example in Haskell:
  - Prelude> elementAt [1,2,3] 2
  - 2
  - Prelude> elementAt "haskell" 5
  - 'e'
  -}
  elementAt :: [a] -> Int -> a
  elementAt [] _ = error "list is empty"
  elementAt _ n | n < 1 = error $ "invalid index:" ++ show n
  elementAt (x:_) 1 = x
  elementAt (_:xs) n = elementAt xs $ n - 1

  {-
  - 4 Problem 4
  - (*) Find the number of elements of a list.
  -
  - Example in Haskell:
  - Prelude> myLength [123, 456, 789]
  - 3
  - Prelude> myLength "Hello, world!"
  - 13
  -}
  myLength :: [a] -> Int
  myLength [] = 0
  myLength (_:xs) = 1 + myLength xs

  {-
  - 5 Problem 5
  - (*) Reverse a list.
  -
  - Example in Haskell:
  - Prelude> myReverse "A man, a plan, a canal, panama!"
  - "!amanap ,lanac a ,nalp a ,nam A"
  - Prelude> myReverse [1,2,3,4]
  - [4,3,2,1]
  -}
  myReverse :: [a] -> [a]
  myReverse xs = myReverse' xs []
    where myReverse' [] ls = ls
          myReverse' (h:ts) ls = myReverse' ts (h:ls)

  {-
  - 6 Problem 6
  - (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
  -
  - Example in Haskell:
  - *Main> isPalindrome [1,2,3]
  - False
  - *Main> isPalindrome "madamimadam"
  - True
  - *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
  - True
  -}
  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome xs = xs == myReverse xs

  {-
  - 7 Problem 7
  - (**) Flatten a nested list structure.
  -
  - Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
  -
  - Example in Haskell:
  - We have to define a new data type, because lists in Haskell are homogeneous.
  -
  -  data NestedList a = Elem a | List [NestedList a]
  -  *Main> flatten (Elem 5)
  -  [5]
  -  *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
  -  [1,2,3,4,5]
  -  *Main> flatten (List [])
  -  []
  -}
  data NestedList a = Elem a | List [NestedList a]
  flatten :: NestedList a -> [a]
  flatten (Elem x) = [x]
  flatten (List []) = []
  flatten (List (x:xs)) = flatten x ++ flatten (List xs)

  {-
  - 8 Problem 8
  - (**) Eliminate consecutive duplicates of list elements.
  -
  - If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  -
  - Example in Haskell:
  - > compress "aaaabccaadeeee"
  - "abcade"
  -}
  compress :: (Eq a) => [a] -> [a]
  compress [] = []
  compress [x] = [x]
  compress (x:y:zs) | x == y = compress $ y:zs
                    | otherwise =  x:(compress $ y:zs)

  {-
  - 9 Problem 9
  - (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
  -
  - Example in Haskell:
  - *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
  -              'a', 'd', 'e', 'e', 'e', 'e']
  -              ["aaaa","b","cc","aa","d","eeee"]
  -}
  pack :: (Eq a) => [a] -> [[a]]
  pack [] = []
  pack (x:xs) = (x:takeWhile (== x) xs):(pack $ dropWhile (== x) xs)

  {-
  - 10 Problem 10
  - (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
  - Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
  -
  - Example in Haskell:
  - encode "aaaabccaadeeee"
  - [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
  -}
  encode :: (Eq a) => [a] -> [(Int, a)]
  encode ls = map (\x -> (length x, head x)) $ pack ls

  {-
  - 1 Problem 11
  - (*) Modified run-length encoding.
  -
  - Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
  - Only elements with duplicates are transferred as (N E) lists.
  -
  - Example in Haskell:
  - P11> encodeModified "aaaabccaadeeee"
  - [Multiple 4 'a',Single 'b',Multiple 2 'c',
  -  Multiple 2 'a',Single 'd',Multiple 4 'e']
  -}
  data Code a = Single a | Multiple Int a
              deriving (Show, Eq)
  encodeModified :: (Eq a) => [a] -> [Code a]
  encodeModified ls = map f $ encode ls
    where f (n, x) | n == 1 = Single x
                   | otherwise = Multiple n x

  {-
  - 2 Problem 12
  - (**) Decode a run-length encoded list.
  -
  - Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
  -
  - Example in Haskell:
  -
  - P12> decodeModified
  -        [Multiple 4 'a',Single 'b',Multiple 2 'c',
  -                Multiple 2 'a',Single 'd',Multiple 4 'e']
  -                "aaaabccaadeeee"
  -}
  decodeModified :: [Code a] -> [a]
  decodeModified [] = []
  decodeModified (Single x:xs) = x:decodeModified xs
  decodeModified (Multiple n x:xs) = multi x n ++ decodeModified xs
    where multi _ 0 = []
          multi y k = x:multi y (k - 1)

  {-
  - 3 Problem 13
  - (**) Run-length encoding of a list (direct solution).
  -
  - Implement the so-called run-length encoding data compression method directly.
  - I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
  - As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
  -
  - Example in Haskell:
  - P13> encodeDirect "aaaabccaadeeee"
  - [Multiple 4 'a',Single 'b',Multiple 2 'c',
  -  Multiple 2 'a',Single 'd',Multiple 4 'e']
  -}
  encodeDirect :: Eq a => [a] -> [Code a]
  encodeDirect [] = []
  encodeDirect ls = foldr f [] ls
    where f x [] = [Multiple 1 x]
          f x (Multiple n y:ys) | x == y = Multiple (n + 1) y:ys
                                | otherwise = Multiple 1 x:Multiple n y:ys
          f _ _ = error "unexpected pattern"

  {-
  - 4 Problem 14
  - (*) Duplicate the elements of a list.
  -
  - Example in Haskell:
  - > dupli [1, 2, 3]
  - [1,1,2,2,3,3]
  -}
  dupli :: [a] -> [a]
  dupli [] = []
  dupli (x:xs) = x:x:dupli xs

  {-
  - 5 Problem 15
  - (**) Replicate the elements of a list a given number of times.
  -
  - Example in Haskell:
  - > repli "abc" 3
  - "aaabbbccc"
  -}
  repli :: [a] -> Int -> [a]
  repli [] _ = []
  repli (x:xs) n | n < 0 = error ("illegal argument n:" ++ show n)
                 | otherwise = foldr (const (x:)) (repli xs n) [1..n]

  {-
  - 6 Problem 16
  - (**) Drop every N'th element from a list.
  -
  - Example in Haskell:
  - *Main> dropEvery "abcdefghik" 3
  - "abdeghk"
  -}
  dropEvery :: [a] -> Int -> [a]
  dropEvery _ n | n <= 0 = error ("illegal argument n:" ++ show n)
  dropEvery ls n = dropEvery' ls n
    where dropEvery' [] _ = []
          dropEvery' (_:xs) 1 = dropEvery' xs n
          dropEvery' (x:xs) k = x:dropEvery' xs (k - 1)

  {-
  - 7 Problem 17
  - (*) Split a list into two parts; the length of the first part is given.
  -
  - Do not use any predefined predicates.
  -
  - Example in Haskell:
  - *Main> split "abcdefghik" 3
  - ("abc", "defghik")
  -}
  split :: [a] -> Int -> ([a], [a])
  split _ n | n < 0 = error ("illegal argument n:" ++ show n)
  split ls n = (take n ls, drop n ls)

  {-
  - 8 Problem 18
  - (**) Extract a slice from a list.
  -
  - Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element
  - of the original list (both limits included). Start counting the elements with 1.
  -
  - Example in Haskell:
  - *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
  - "cdefg"
  -}
  slice :: [a] -> Int -> Int -> [a]
  slice _ i j | j < i = []
  slice ls i j = drop (i - 1) $ take j ls

  {-
  - 9 Problem 19
  - (**) Rotate a list N places to the left.
  -
  - Hint: Use the predefined functions length and (++).
  -
  - Examples in Haskell:
  - *Main> rotate ['a','b','c','d','e','f','g','h'] 3
  - "defghabc"
  -
  -  *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
  -  "ghabcdef"
  -}
  rotate :: [a] -> Int -> [a]
  rotate [] _ = []
  rotate ls 0 = ls
  rotate ls n | length ls < n = rotate ls $ mod n $ length ls
              | n < 0 = rotate ls $ n + length ls
              | otherwise = (drop n ls) ++ (take n ls)

  {-
  - 10 Problem 20
  - (*) Remove the K'th element from a list.
  -
  - Example in Haskell:
  - *Main> removeAt 2 "abcd"
  - ('b',"acd")
  -}
  removeAt :: Int -> [a] -> (a, [a])
  removeAt n ls | length ls == 0 = error "list is empty"
                | length ls < n = error $ "illegal argument n:" ++ show n
                | n <= 0 = error $ "illegal argument n:" ++ show n
                | otherwise = (head ls2, ls1 ++ tail ls2)
    where (ls1, ls2) = split ls $ n - 1

  {-
  - 1 Problem 21
  - Insert an element at a given position into a list.
  -
  - Example in Haskell:
  - P21> insertAt 'X' "abcd" 2
  - "aXbcd"
  -}
  insertAt :: a -> [a] -> Int -> [a]
  insertAt e [] _ = [e]
  insertAt e (x:xs) n | n <= 1 = e:x:xs
                      | otherwise = x:insertAt e xs (n - 1)

  {-
  - 2 Problem 22
  - Create a list containing all integers within a given range.
  -
  - Example in Haskell:
  - Prelude> range 4 9
  - [4,5,6,7,8,9]
  -}
  range :: Int -> Int -> [Int]
  range s e | e < s = []
            | s == e = [e]
            | otherwise = s:range (s + 1) e

  {-
  - 3 Problem 23
  - Extract a given number of randomly selected elements from a list.
  -
  - Example in Haskell:
  - Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
  - eda
  -}
  rnd_select :: [a] -> Int -> IO [a]
  rnd_select _ 0 = return []
  rnd_select [] _ = error "list is empty"
  rnd_select ls n | n < 0 = error $ "illegal argument n:" ++ show n
                  | otherwise = do pos <- replicateM n $ getStdRandom $ randomR (0, length ls - 1)
                                   return [ls !! p | p <- pos]

  {-
  - 4 Problem 24
  - Lotto: Draw N different random numbers from the set 1..M.
  -
  - Example in Haskell:
  - Prelude System.Random>diff_select 6 49
  - Prelude System.Random>[23,1,17,33,21,37]
  -}
  diff_select :: Int -> Int -> IO [Int]
  diff_select n m | n < 0 = error $ "illegal argument n:" ++ show n
                  | m <= 0 = error $ "illegal argument m:" ++ show m
                  | otherwise = rnd_select [1..m] n

  {-
  - 5 Problem 25
  - Generate a random permutation of the elements of a list.
  -
  - Example:
  -
  - * (rnd-permu '(a b c d e f))
  - (B A D C E F)
  - Example in Haskell:
  -
  - Prelude System.Random>rnd_permu "abcdef"
  - Prelude System.Random>"badcef"
  -}
  rnd_permu :: [a] -> IO [a]
  rnd_permu ls = rnd_select ls $ length ls

  {-
  - 6 Problem 26
  - (**) Generate the combinations of K distinct objects chosen from the N elements of a list
  -
  - In how many ways can a committee of 3 be chosen from a group of 12 people?
  - We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
  - For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
  -
  - Example in Haskell:
  - > combinations 3 "abcdef"
  - ["abc","abd","abe",...]
  -}
  combinations :: Int -> [a] -> [[a]]
  combinations r ls | r < 0 = error $ "illegal argument r:" ++ show r
                    | r > length ls = []
                    | r == 0 = [[]]
                    | otherwise = (map (x:) $ combinations (r - 1) xs) ++ combinations r xs
    where (x:xs) = ls

  {-
  - 7 Problem 27
  - Group the elements of a set into disjoint subsets.
  -
  - a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
  -
  - b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
  -
  - Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...).
  - However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
  -
  - You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
  -
  - Example in Haskell:
  - P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
  - [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
  - (altogether 1260 solutions)
  -
  -  27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
  -  [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
  -  (altogether 756 solutions)
  -}
  group3 :: Eq a => [a] -> [[[a]]]
  -- group3 ls = concatMap group2 $ combinations 2 ls
  --   where group2 g1 = map (\g2 -> [g1, g2, remove_all g2 g1]) $ combinations 3 xs
  --           where xs = remove_all g1 ls
  --                 remove_all _ [] = []
  --                 remove_all [] ys = ys
  --                 remove_all (e:es) ys = filter (/= e) $ remove_all es ys
  group3 ls = group [2, 3, 4] ls

  group :: Eq a => [Int] -> [a] -> [[[a]]]
  group [] _ = [[]]
  group ns ls | sum ns /= length ls = error "Cannot create groups"
              | minimum ns < 0 = error "Cannot create groups"
              | otherwise = concatMap group2 $ combinations n ls
    where (n:ms) = ns
          group2 g1 = map (g1:) $ group ms xs
            where xs = remove_all g1 ls
                  remove_all _ [] = []
                  remove_all [] ys = ys
                  remove_all (e:es) ys = filter (/= e) $ remove_all es ys

  {-
  - Sorting a list of lists according to length of sublists
  -
  - a) We suppose that a list contains elements that are lists themselves.
  - The objective is to sort the elements of this list according to their length.
  - E.g. short lists first, longer lists later, or vice versa.
  -
  - Example in Haskell:
  - Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
  - Prelude>["o","de","de","mn","abc","fgh","ijkl"]
  -
  - b) Again, we suppose that a list contains elements that are lists themselves.
  - But this time the objective is to sort the elements of this list according to their length frequency;
  - i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
  -
  - Example in Haskell:
  - lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
  - ["ijkl","o","abc","fgh","de","de","mn"]
  -}
  lsort :: [[a]] -> [[a]]
  lsort ls = sortBy (\x y -> compare (length x) (length y)) ls
