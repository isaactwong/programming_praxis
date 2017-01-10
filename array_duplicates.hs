import Data.List

{-
23 September 2011

We have today another exercise from our large file of interview questions:

You are given an array with integers between 1 and 1,000,000. One integer is in the array twice. How can you determine which one?

Your task is to write code to solve the array duplicates problem. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.
-}

array_dup :: [Int] -> Int
array_dup = f . sort 
          where f (x:y:xs) = if x == y then x else f (y : xs)