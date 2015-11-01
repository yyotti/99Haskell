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
