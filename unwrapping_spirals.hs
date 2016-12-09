import Data.List

-- 01Jun2010
-- The task is to enumerate the elements of a matrix in spiral order. For instance, consider the matrix:

--  1  2  3  4
--  5  6  7  8
--  9 10 11 12
-- 13 14 15 16
-- 17 18 19 20

-- The spiral starts across the first row, yielding 1, 2, 3, and 4. Then the spiral turns right and runs down the right column, yielding 8, 12, 16, and 20. The spiral turns right again and runs across the bottom row, from right to left, yielding 19, 18, and 17. Then up the first column with 13, 9, 5, right with 6 and 7, down with 11 and 15, right to 14, and up to 10. Thus, unwrapping the given matrix in a spiral gives the list of elements 1, 2, 3, 4, 8, 12, 16, 20, 19, 18, 17, 13, 9, 5, 6, 7, 11, 15, 14, and 10.

-- Your task is to write a function to unwrap spirals. 


-- Always take the first row as is, then rotate the matrix counterclockwise (reverse . transpose OR transpose . reverse) and then recurse through a smaller matrix.
unspiral :: [[a]] -> [a]
unspiral [] = []
unspiral (x : xs) = x ++ unspiral (transpose (map reverse xs))


