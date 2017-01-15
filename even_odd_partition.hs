{-
4 May 2012

I’m not sure where this problem comes from; it’s either homework or an interview question. Nonetheless, it is simple and fun:

Take an array of integers and partition it so that all the even integers in the array precede all the odd integers in the array. Your solution must take linear time in the size of the array and operate in-place with only a constant amount of extra space.

Your task is to write the indicated function.
-}

parity_partition :: [Int] -> [Int]
parity_partition [] = []
parity_partition xs = [x | x <- xs, odd x] ++ [x | x <- xs, even x]