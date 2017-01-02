import Data.List

{-
19 July 2011

Write a program that takes a list of integers and a target number and determines if any two integers in the list sum to the target number. If so, return the two numbers. If not, return an indication that no such integers exist.
-}

sum_of_two :: Int -> [Int] -> Maybe (Int, Int)
sum_of_two _ [] = Nothing
sum_of_two n xs = find f [((xs!!i),(xs!!j)) | i <- [0..m], j <- [0..m], i /= j]
           where m = (length xs)-1
                 f (a,b) = a+b == n
