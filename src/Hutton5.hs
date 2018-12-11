module Hutton5 where

-- Chapter 5 exercises

-- Using a list comprehension, give an expression that calculates the sum 1² + 2² + ...
-- 100² of the first one hundred integer squares.
hut1 :: Int
hut1 = sum [x | x <- [1 .. (100 ^ 2)]]

-- Suppose that a coordinate grid of size m × n is given by the list of all pairs (x, y) of
-- integers such that Using a list comprehension, define a
-- function grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid of a
-- given size. For example:
-- > grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
hut2 :: Int -> Int -> [(Int, Int)]
hut2 x y = [(a, b) | a <- [0..x], b <- [0..y] ]

-- Using a list comprehension and the function grid above, define a function square
-- :: Int -> [(Int,Int)] that returns a coordinate square of size n, excluding the
-- diagonal from (0, 0) to (n, n). For example:
-- > square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
hut3 :: Int -> [(Int,Int)]
hut3 x = [(a, b) | a <- [0..x], b <- [0..x] , a /= b ]

-- In a similar way to the function length , show how the library function replicate
-- :: Int -> a -> [a] that produces a list of identical elements can be defined using
-- a list comprehension. For example:
-- > replicate 3 True
-- [True,True,True]
hut4 :: Int -> a -> [a]
hut4 0 i = []
hut4 n i = i : (hut4 (n-1) i)

-- A triple (x, y, z) of positive integers is Pythagorean if it satisfies the equation x² + y² = z²
-- Using a list comprehension with three generators, define a function pyths ::
-- Int -> [(Int,Int,Int)] that returns the list of all such triples whose components
-- are at most a given limit. For example:
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x*x + y*y == z*z)]

-- 6. A positive integer is perfect if it equals the sum of all of its factors, excluding the
-- number itself. Using a list comprehension and the function factors, define a function
-- perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given
-- limit. For example:
-- > perfects 500
-- [6,28,496]
perfects :: Int -> [Int]
perfects n = let perf e = [x | x <- [1..(e-1)], e `mod` x == 0]
  in [x | x <- [1..n], sum (perf x) == x ]

-- 9. The scalar product of two lists of integers xs and ys of length n is given by the sum
-- of the products of corresponding integers:
-- [$]\sum _{i=0}^{n-1}(xs_i * ys_i)[/$]
-- In a similar manner to chisqr , show how a list comprehension can be used to define
-- a function scalarproduct :: [Int] -> [Int] -> Int that returns the scalar
-- product of two lists. For example:
-- > scalarproduct [1,2,3] [4,5,6]
-- 32

-- 10. Modify the Caesar cipher program to also handle upper-case letters.