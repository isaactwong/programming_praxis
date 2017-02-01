import Data.List

{-
15 Oct 2017
We have today another exercise from our limitless supply of interview questions:

You are given two arrays of integers, where the integers do not repeat, the two arrays have no common integers, and both arrays are sorted in ascending order.

Let x be any integer in the first array and y be any integer in the second array. Find min(abs(x − y)); i.e., find the smallest difference between any integers in the two arrays.

Your task is to write a program to find the smallest difference.
-}

-- Bad performance as it ignores all of the structure in the arrays. Also requires non-empty arrays.
min_diff :: [Int] -> [Int] -> ((Int,Int), Int)
min_diff xs ys = head (sortBy (\(_,d) (_,d') -> compare d d') [((i,j),abs(i-j)) | i<-xs, j<-ys])

-- Better version...with help from programming praxis comments.
min_diff' :: [Int] -> [Int] -> Maybe Int
min_diff' [] _ = Nothing
min_diff' _ [] = Nothing
min_diff' xs ys = Just (go xs ys (abs((head xs)-(head ys))))
          where
                go [] _ d = d
                go _ [] d = d
                go (l:ls) (r:rs) d
                   | d' == 1   = 1
                   | l < r     = go ls (r:rs) d'
                   | otherwise = go (l:ls) rs d'
                     where d'  = min d (abs r-l)
                