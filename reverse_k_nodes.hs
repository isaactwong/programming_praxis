{-
26 August 2011

Given a list of elements and a block size k, return the list of elements with every block of k elements reversed, starting from the beginning of the list. For instance, given the list 1, 2, 3, 4, 5, 6 and the block size 2, the result is 2, 1, 4, 3, 6, 5.
-}

reverse_k :: Int -> [a] -> [a]
reverse_k k [] = []
reverse_k k xs = (reverse (take k xs)) ++ (reverse_k k (drop k xs))