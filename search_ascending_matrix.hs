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

-}

ys = [[1, 5,  7,  9],
     [4, 6,  10, 15],
     [8, 11, 12, 19],
     [4, 16, 18, 21]]

find_coordinates :: (Ord a) => a -> [[a]] -> Maybe (Int, Int)
find_coordinates k xs = do
                                (r, cs) <- find in_row ms
                                (c, value) <- find in_column cs
                                return (c,r)
                            where
                                ms = zip [1..] $ map (zip [0..]) xs -- index the array (row, [(column, value)])
                                in_row (c,rs) = (head rs) <= k && k <= (last rs)
                                in_column (i,x) = k == x

