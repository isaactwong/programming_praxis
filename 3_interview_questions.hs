{-
23 August 2013

1) Verify that a binary tree is a binary search tree. In a binary search tree, all nodes to the left of the current node have values less than the value of the current node, all nodes to the right of the current node have values greater than the value of the current node, and those two rules hold for all nodes in the tree.

2) Remove duplicates from a list.

3) A girl counts on her fingers by counting 1 on her thumb, 2 on her index finger, 3 on her middle finge, 4 on her ring finger, and 5 on her pinkie. Then she continues the other way, counting 6 on her ring finger, 7 on her middle finger, 8 on her index finger, and 9 on her thumb. Then she continues the other other way, counting 10 on her index finger, 11 on her middle finger, 12 on her ring finger, and 13 on her pinkie. She continues in this way indefinitely. Write a program that, given n, determines which finger she will be on when her count reaches n.
-}

data BinaryTree a = Empty | Branch a (BinaryTree a) (BinaryTree a) deriving (Show)
is_binary :: (Ord a) => (BinaryTree a) -> Bool
is_binary t = and (zipWith (<=) io (tail io))
          where in_order Empty = []
                in_order (Branch x l r) = (in_order l) ++ [x] ++ (in_order r)
                io = in_order t

dups :: (Eq a) => [a] -> [a]
dups (x:xs) = x : (dups $ filter (/=x) xs)
dups [] = []

finger :: Int -> Char
finger 1 = 't'
finger n = head $ drop (n-1) (concat . repeat $ "timrprmi")
