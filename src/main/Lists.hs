module Lists where

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
