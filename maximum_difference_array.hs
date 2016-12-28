import Data.List

{-

1 April 2011

Given an array X, find the j and i that maximizes Xj − Xi, subject to the condition that i ≤ j. If two different i,j pairs have equal differences, choose the “leftmost shortest” pair with the smallest i and, in case of a tie, the smallest j.

For instance, given an array [4, 3, 9, 1, 8, 2, 6, 7, 5], the maximum difference is 7 when i=3 and j=4. Given the array [4, 2, 9, 1, 8, 3, 6, 7, 5], the maximum difference of 7 appears at two points, but by the leftmost-shortest rule the desired result is i=1 and j=2. I and j need not be adjacent, as in the array [4, 3, 9, 1, 2, 6, 7, 8, 5], where the maximum difference of 7 is achieved when i=3 and j=7. If the array is monotonically decreasing the maximum difference is 0, which by the leftmost-shortest rule occurs when i=0 and j=0.

There are at least two solutions. The obvious solution that runs in quadratic time uses two nested loops, the outer loop over i from 0 to the length of the array n and the inner loop over j from i+1 to n, computing the difference between Xi and Xj and saving the result whenever a new maximum difference is found. There is also a clever linear-time solution that traverses the array once, simultaneously searching for a new minimum value and a new maximum difference; you’ll get it if you think about it for a minute.

Your task is to write both the quadratic and linear functions to compute the maximum difference in an array, and also a test function that demonstrates they are correct.
-}

diff_max :: [Int] -> (Int, Int)
diff_max xs = fst (maximumBy (\((x,y),d) ((a,b),c) -> compare d c) ds)
         where ds = [((i,j), (xs!!j)-(xs!!i)) | i <- [0..n], j <- [i..n]]
               n = (length xs)

fast_diff_max :: [Int] -> (Int, Int)
-- The accumulater is (min_seen_so_far, (i,j)) where x_j-x_i is maximized

fast_diff_max xs = snd $ foldl (\(m,(i,j)) (l,x) -> f (m,(i,j)) (l,x)) (0,(0,0)) xis
              where xis = zip [0..] xs
                    f (m,(i,j)) (l,x) =
                      (   if x < (xs!!m) then l else m,
                          if x-(xs!!m) > (xs!!j)-(xs!!i) then (m,l) else (i,j)
                      )


 