{-
19 September 2017

Since our exercise a week ago, I’ve been reading about Catalan numbers, primarily based on the references at OEIS. Catalan’s Triangle (A009766) is a number triangle

1
1 1
1 2  2
1 3  5  5
1 4  9 14 14
1 5 14 28 42  42
1 6 20 48 90 132 132
such that each element is equal to the one above plus the one to the left.

Your task is to write a program that calculates a Catalan triangle.
-}

recurrence :: Int -> Int -> Int
recurrence _ 0 = 1
recurrence  n k 
  | n == k = recurrence n (k-1)
  | otherwise  = (recurrence n (k-1)) + (recurrence (n-1) k)

catalan_triangle :: [[Int]]
catalan_triangle = map (\x -> map (recurrence x) [0..x]) [0..]

print_catalan_triangle :: Int -> IO ()
print_catalan_triangle n = mapM_ print (take n catalan_triangle)
