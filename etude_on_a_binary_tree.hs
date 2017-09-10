{-
25 April 2017

We have today another in our occasional series of exercises on binary trees; the input tree need not necessarily be ordered or balanced:

Given a binary tree containing integers, find the sum of all nodes at an even distance from the root, less the sum of all nodes at an odd distance from the root.

For instance, given the binary tree shown below, the requested sum is 1 – 2 – 3 + 4 + 5 + 6 + 7 – 8 – 9 – 10 – 11 – 12 – 13 – 14 – 15 = -74:

           1
     2           3
  4     5     6     7
 8  9 10 11 12 13 14 15
(I’m not an artist; you’ll have to imagine the lines connecting the various levels.)

Your task is to write a program to compute alternate sums and differences of the nodes of a binary tree.
-}

data Tree a =  Empty | Node a (Tree a) (Tree a) deriving (Show)

tree = Node 1
       (
         Node 2 (Node 4 (Node 8 Empty Empty) (Node 9 Empty Empty)) (Node 5 (Node 10 Empty Empty) (Node 11 Empty Empty))
       )
       (
         Node 3 (Node 6 (Node 12 Empty Empty) (Node 13 Empty Empty)) (Node 7 (Node 14 Empty Empty) (Node 15 Empty Empty))
       )

alternatingTreeSum :: Tree Integer -> Integer
alternatingTreeSum Empty               = 0
alternatingTreeSum (Node x left right) = x - (alternatingTreeSum left) - (alternatingTreeSum right)
