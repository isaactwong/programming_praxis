import qualified Data.IntMap as I

{-

We have today another question from our inexhaustible set of interview questions; this one comes from Amazon:

Given a million points (x, y), give an O(n) solution to find the 100 points closest to (0, 0).

Your task is to write the requested function.

-}

-- Bonsai Code introduces IntMaps that are O(1) insert and O(n) sort. Probably using something like Radix sort.
-- Use the squared euclidean norm as the key.
-- https://bonsaicode.wordpress.com/2012/11/27/programming-praxis-amazon-interview-question/

nearest_neighbors :: Int -> [(Int,Int)] -> [(Int,Int)]
nearest_neighbors n xs = take n (concat (I.elems (I.fromListWith (++) ds)))
                  where
                        ds = map (\(x,y) -> (x*x + y*y, [(x,y)])) xs