
-- https://wiki.haskell.org/99_questions/1_to_10

module HS00 where
import Data.List

t1 = [9,1,7,2,3,8,10,4,5,6]
t2 = [2]
t3 = [2, 5]
t4 = "Depeche Mode Fragile tension"
t5 = "сукификус"
t6 = []
t7 = "aaaabccaadeeee"
t8 = [1,1,1,1,2,3,3,1,1,4,5,5,5,5]

-- Problem 1
-- (*) Find the last element of a list.
-- (Note that the Lisp transcription of this problem is incorrect.)
-- myLast [1,2,3,4]
-- 4
-- myLast ['x','y','z']
-- 'z'
p01 l = head $ reverse l

-- Problem 2
-- (*) Find the last but one element of a list.
-- (Note that the Lisp transcription of this problem is incorrect.)

-- myButLast [1,2,3,4]
-- 3
-- myButLast ['a'..'z']
-- 'y'
p02 l = drop 1 (take 2 (reverse l))

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- Example:
-- * (element-at '(a b c d e) 3)
-- c
p03 k l = head (drop (k-1) l)
p03_ k l = l !! (k-1)

-- Problem 4
-- (*) Find the number of elements of a list.
-- myLength [123, 456, 789]
-- 3
-- myLength "Hello, world!"
-- 13
p04 l = sum [1 | _ <- l]
p04_ [] = 0
p04_ l = 1 + p04_ (tail l)

-- (*) Reverse a list.
-- myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- myReverse [1,2,3,4]
-- [4,3,2,1]
p05 [] = []
p05 (x:xs) = (p05 xs) ++ [x]

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
-- isPalindrome [1,2,3]
-- False
-- isPalindrome "madamimadam"
-- True
-- isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
p06 l = (l == (p05 l))

-- Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)

data NestedList a = Elem a | List [NestedList a]
p07 :: NestedList a -> [a]
p07 (Elem a) = [a]
p07 (List []) = []
p07 (List (a:as)) = p07 a ++ p07 (List as)

-- Problem 8
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
p08 :: Eq a => [a] -> [a]
p08 [] = []
p08 (x:xs) = if (length xs >= 1 && x == (head xs)) then p08 xs else [x] ++ p08 xs

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
p09 :: Eq x => [x] -> [[x]]
p09 [] = []
p09 l = let (x, y) = span (==head l) l in [x] ++ p09 y

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

p10 :: Eq x => [x] -> [(Int, x)]
p10 [] = []
p10 l = let (x, y) = span (==head l) l in [(length x, head x)] ++ p10 y