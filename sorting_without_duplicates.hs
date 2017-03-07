{-
27 Feb 2017

Given an array of n integers, partition the array into sub-arrays, each in ascending order, and each without duplicates. For instance, given the array [2, 9, 1, 5, 1, 4, 9, 7, 2, 1, 4], the desired output is the array [1, 2, 4, 5, 7, 9,   1, 2, 4, 9,   1], where the intent is to have as many sub-arrays as the maximum frequency of any element, each sub-array as long as possible before starting the next sub-array of duplicates. If possible, perform the work in-place, in time either O(n) or O(n log n).

Very cool solution from Kevin on Programming Praxis!
-}

import Data.List
sort' :: (Ord a) => [a] -> [a]
sort' = concat . transpose . group . sort 

