import Data.Array
import Data.List

{-
10 February 2012

Given an m by n matrix of integers with each row and column in ascending order, search the matrix and find the row and column where a key k appears, or report that k is not in the matrix. For instance, in the matrix

 1  5  7  9
 4  6 10 15
 8 11 12 19
14 16 18 21

the key 11 appears in row 2, column 1 (indexing from 0) and the key 13 is not present in the matrix. The obvious algorithm takes time O(m × n) to search the matrix row-by-row, but you must exploit the order in the matrix to find an algorithm that takes time O(m + n).

Your task is to write the requested search function. 

Stole the solution from one of the comments in programming praxis. listArrays are very cool!

-}

ys = [[1, 5,  7,  9],
     [4, 6,  10, 15],
     [8, 11, 12, 19],
     [4, 16, 18, 21]]

as = listArray ((0,0),(3,3)) [1,5,7,9,4,6,10,15,8,11,12,19,4,16,18,21]

find_coordinates :: (Ord a) => a -> Array (Int,Int) a -> Maybe (Int, Int)
find_coordinates x m = find' 0 (snd . snd . bounds $ m) m x
find' i j m x
      | i < 0 = Nothing
      | j < 0 = Nothing
      | i > (fst . snd . bounds $ m) = Nothing
      | j > (snd . snd. bounds $ m)  = Nothing
      | x == (m ! (i,j))             = Just (i,j)
      | x < (m ! (i,j))              = find' i (j-1) m x
      | x > (m ! (i,j))              = find' (i+1) j m x
