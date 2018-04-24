
-- https://wiki.haskell.org/99_questions/11_to_20

module HS10 where
import Data.Foldable
import Data.List

t1 = [9,1,7,2,3,8,10,4,5,6]
t2 = [2]
t3 = [2, 5]
t4 = "Depeche Mode Fragile tension"
t5 = "сукификус"
t6 = []
t7 = "aaaabccaadeeee"
t8 = [1,1,1,1,2,3,3,1,1,4,5,5,5,5]

-- Problem 11
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. 
data Jopa a = JJ Int a | J a 
  deriving Show
p11 :: Eq x => [x] -> [Jopa x]
p11 [] = []
p11 l = let (x, y) = span (== head l) l
  in if length x == 1
    then [J (head x)] ++ p11 y 
    else [JJ (length x) (head x)] ++ p11 y

-- Problem 12
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
p12 :: [Jopa a] -> [a]
p12 a = concat $ map (f) a
  where
    f (J b) = [b]
    f (JJ n b) = replicate n b

-- Problem 13
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

p13 = "fuck it"

-- Problem 14
-- Duplicate the elements of a list.
-- > dupli [1, 2, 3]
-- [1,1,2,2,3,3]
-- Problem 15
-- Replicate the elements of a list a given number of times.
p1415 :: [a] -> Int -> [a]
p1415 [] n = []
p1415 a n = (replicate n (head a)) ++ p1415 (tail a) n

p14' :: [a] -> [a]
p14' = concat . map dup
  where
    dup x = [x,x]

-- Problem 16
-- Drop every N'th element from a list.
p16 :: [a] -> Int -> [a]
p16 [] _ = []
p16 _ 0 = []
p16 _ 1 = []
p16 l n = (take (n-1) l) ++ p16 (drop n l) n

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))
p17 l n = splitAt n l

-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
-- * (slice '(a b c d e f g h i k) 3 7)
-- (C D E F G)
p18 :: [a] -> Int -> Int -> [a]
p18 list l r = drop (l-1) (take (r) list)

-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
p19 :: [a] -> Int -> [a]
p19 l w = drop w l ++ (take w l) 

-- Problem 20
-- Remove the K'th element from a list.
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")
p20 :: [a] -> Int -> (a, [a])
p20 l k = let w = splitAt k l in (last $ fst w, (init $ fst w) ++ (snd w))