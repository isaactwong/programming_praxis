import Data.List

-- 6 Dec 2016
-- Given two sorted arrays, efficiently find the median of the combined array.
median :: (Floating a, Ord a) => [a] -> [a] -> a
median rs ls = med $ rs ++ ls


med :: (Floating a, Ord a) => [a] -> a
med xs
    | even l       = head (drop (l `div` 2) sxs)
    | otherwise    = avg (take 2 (drop (l `div` 2) sxs))
  where
        l   = (length xs) - 1
        sxs = sort xs
        avg as = ((as !! 0) + (as !! 1))/2