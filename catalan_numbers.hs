{-
12 Sept 2017

Today’s exercise is an Amazon interview question for software engineers and developers:

How many unique binary search trees can be made from a series of numbers 1, 2, 3, 4, …, n, for any given n?
-}

import Control.Monad
import Data.List

data BinaryTree = Empty | Node BinaryTree BinaryTree deriving Show


-- The algorithm is recursive. To get a tree of size n, take any two trees, say s and t, with n-m and m nodes respectively, and create a new tree out of those two by (Node s t). We then need to do this for all combinations of trees whose sizes add up to n and we need to include (Node s t) and (Node t s). 
-- So create an infinite list of lists of trees of size n, or [[size 0 trees], [size 1 trees], ..., [size n trees], ...]. Then you need to create all possible pairs of trees whose size adds to n+1.
-- The very cool implementation of this was found at https://stackoverflow.com/questions/28100650/generate-all-possible-trees and leans heavily on lazy evaluation.

binaryTrees :: [[BinaryTree]]
binaryTrees = [Empty] : map fuseSubTrees (drop 1 . inits $ binaryTrees)
  where
    fuseSubTrees smaller = do
       (ls, rs) <- zip smaller (reverse smaller)
       liftM2 Node ls rs

-- To find all unique binary search trees <= n is then
-- map length (take n binaryTrees)
