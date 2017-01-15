import qualified Data.IntMap as I

{-
14 August 2012

We have today another exercise from our inexhaustible stock of interview questions:

Given an array of integers, output a list of four integers that sum to zero (the same input integer can be used multiple times), or indicate that no such set of four integers exists. For example, given the array
(2 3 1 0 -4 -1), the set of four integers (3 1 0 -4) sums to zero, as does the set (0 0 0 0).

Your task is to write a program that solves the interview question.

-}

fours :: [Int] -> [(Int, Int, Int, Int)]
fours xs = [(i,j,k,l) | ((i,j),s) <- pairs, ((k,l),s') <- pairs, s == -s', i<=j && j<=k && k<=l]
      where pairs = [((i,j),i+j) | i <- xs, j <- xs]

-- And a fancy one from Bonsai Code
-- https://bonsaicode.wordpress.com/2012/08/14/programming-praxis-4sum/
sum4 :: Int -> [Int] -> [[Int]]
sum4 n xs = [p1++p2 | (s,p1) <- I.assocs pairs, p2 <- maybe [] return (I.lookup (n-s) pairs)]
     where pairs = I.fromList [(x+y,[x,y]) | x <- xs, y <- xs]