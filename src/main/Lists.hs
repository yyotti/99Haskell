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
  myButLast (_:[]) = error "list has only one element"
  myButLast (x:_:[]) = x
  myButLast (_:xs) = myButLast xs
