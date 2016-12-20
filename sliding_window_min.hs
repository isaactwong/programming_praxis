{-
The sliding window minimum problem takes a list of n numbers and a window size k and returns a list of the minimum values in each of the n−k+1 successive windows. For instance, given the list {4, 3, 2, 1, 5, 7, 6, 8, 9} and a window of size 3, the desired output is the list {2, 1, 1, 1, 5, 6, 6}. Richard Harter discusses this problem at his blog, along with several different solutions.

The obvious solution is to report the minimum of the first k elements of the list, slide one position down the list, take the minimum of the first k elements starting at the new position, and so on, until there are less than k elements remaining.

Harter presents a better solution that he calls the ascending minima algorithm that requires O(n) time and O(k) space. He uses an auxiliary data structure, a queue, that is initialized as the minimum value of the initial window, followed by the minimum value of those items in the initial window that follow the right-most occurrence of the minimum value, followed by the minimum value of those items in the initial window that follow the right-most occurrence of the second value, and so on, up to a maximum of k ascending minimums; each minimum is paired with the index of the position where the minimum disappears from the window. For instance, with k=6 and the first six items of the input list {5, 2, 8, 6, 4, 7}, the queue will have three pairs (2 7), (4 10), and (7 11), indicating that 2 is the minimum value up to and including the 7th list element, then 4 is the minimum value for the 8th, 9th and 10th elements, and so on, unless a smaller item appears.

Once the queue of ascending minimums is initialized, output is produced by emitting the first value in the queue then updating the queue with the next item beyond the end of the current sliding window using the following three-step process:

remove from the queue all items with value greater than the incoming item,
append the incoming item to the end of the queue, along with its “death index,” and
remove the head of the queue if it is beyond its death index.

Let’s look again at the list {4, 3, 2, 1, 5, 7, 6, 8, 9} with a window of size 3. The initial queue has the single entry (2 5), because the minimum item is the last element of the initial window (the largest possible queue arises when the window has monotonically increasing values); the queue entry indicates that the minimum value will be 2 until after the 5th item in the input, when that minimum value “dies.” The initial minimum 2 is output and the new item 1 is added to the queue; since 2 is greater than 1, the (2 5) entry is removed from the queue, a new (1 6) entry is added to the end of the queue, and since the current index 4 is less than the death index 6, we proceed to the next item. Now we output the current minimum 1, add an entry (5 7) to the queue, and move on. Again we output the current minimum 1, add an entry (6 8) to the queue, and move on. One more time we output the current minimum 1, and an entry (8 9) to the queue, and delete (1 6) from the head of the queue since the current index has reached its death index; the queue currently contains the items (5 7) (6 8) and (8 9). Going forward, we output 5 and delete the head of the queue, output 6, and output 6 again, at which point we stop because the current index has reached the end of the input.

Your task is to write two functions that solve the sliding window minimum problem, using the two algorithms described above. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.
-}

sliding_window_slow :: Int -> [Int] -> [Int]
sliding_window_slow _ [] = []
sliding_window_slow k xs = map minimum (windows k xs)

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows k xs = [(take k xs)] ++ windows k (drop 1 xs)
