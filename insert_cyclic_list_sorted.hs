{-

16 August 2011

Given a cyclic list with elements in sorted order, write a function to insert a new element into the cyclic list that maintains the sorted order of the cyclic list. Do not assume that the cyclic list is referenced by its minimal element.

-}

-- Circular, doubly-linked list based on the standard list type.
-- from https://wiki.haskell.org/Tying_the_Knot#Other_Examples
-- This is an amazing use of lazy evaluation!
data DList a = DLNode (DList a) a (DList a) deriving (Show)

mkDList :: [a] -> DList a
mkDList [] = error "must have at least one element"
mkDList xs = let (first,last) = go last xs first
             in first

go :: DList a -> [a] -> DList a -> (DList a, DList a)
go prev [] next = (next, prev)
go prev (x:xs) next = let this        = DLNode prev x rest
                          (rest,last) = go this xs next
                      in (this,last)

takeF :: Integer -> DList a -> [a]
takeF 0 _ = []
takeF n (DLNode _ x next) = x : (takeF (n-1) next)

takeR :: Integer -> DList a -> [a]
takeR 0 _ = []
takeR n (DLNode previous x _) = x : (takeR (n-1) previous)
