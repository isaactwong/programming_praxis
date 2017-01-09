{-
10 Nov 2009

The last O(n log n) sorting algorithm that we shall consider in our current series of exercises is merge sort. If you have two sorted sequences, they can be merged into a single sorted sequence in time linear to their combined length by running through them in order, at each step taking the smaller of the heads of the two sequences. Then mergesort works by recursively merging smaller sequences into larger ones, starting with trivially-sorted sequences of one element that are merged into two-element sequences, then merging pairs of two-element sequences into four-element sequences, and so on, until the entire sequence is sorted.

Your task is to write a function that sorts an array by the merge sort algorithm described above, according to the conventions of the prior exercise. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.
-}

-- Merge two sorted list together by repeatedly sorting on the head elements.
merge_sort :: (Ord a) => [a] -> [a] -> [a]
merge_sort [] ys = ys
merge_sort xs [] = xs
merge_sort (x:xs) (y:ys)
           | x <= y     = x : (merge_sort xs (y:ys))
           | otherwise  = y : (merge_sort (x:xs) ys)


-- Sort an unsorted array.
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge_sort (msort left) (msort right)
      where (left, right) = splitAt (div (length xs) 2) xs