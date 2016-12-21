{-
11 March 2011

Given a binary tree t and two elements of the tree, m and n, with m<n, find the lowest element of the tree (farthest from the root) that is an ancestor of both m and n.

For example, in the tree shown at right, the lowest common ancestor of 4 and 7 is 6, the lowest common ancestor of 4 and 10 is 8, and the lowest common ancestor of 1 and 4 is 3. It is possible for an element of the tree to be its own ancestor, so the lowest common ancestor of 1 and 3 is 3, and the lowest common ancestor of 3 and 6 is 3.

Your task is to write a function that finds the lowest common ancestor of two elements of a binary tree. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.
-}

data Tree a = Empty | Branch a (Tree a) (Tree a)
tree = Branch 8 (Branch 3 (Branch 1 Empty Empty) (Branch 6 (Branch 4 Empty Empty) (Branch 7 Empty Empty)))
                (Branch 10 (Empty) (Branch 14 (Branch 13 Empty Empty) Empty))

lca :: (Ord a) => a -> a -> Tree a -> a
lca x y tree = fst . last $ (takeWhile (\(a,b) -> a==b) (zip (ancestor x tree) (ancestor y tree)))

ancestors :: (Ord a) => a -> Tree a -> [a]
ancestors x Empty = []
ancestor x (Branch y left right) = [y] ++ (if x<y
                                              then ancestor x left
                                              else if x>y
                                                   then ancestor x right
                                                   else [])

-- Clever one. The parent is the first place where the two desired elements are on opposite side of the node. Stolen from the Programming Praxis site.
-- We are assuming m < n
lca' :: (Ord a) => a -> a -> Tree a -> Maybe a
lca' m n Empty = Nothing
lca' m n (Branch x left right)
     | n < x = lca' m n left
     | m > x = lca' m n right
     | otherwise = Just x