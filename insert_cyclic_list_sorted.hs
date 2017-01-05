{-

16 August 2011

Given a cyclic list with elements in sorted order, write a function to insert a new element into the cyclic list that maintains the sorted order of the cyclic list. Do not assume that the cyclic list is referenced by its minimal element.

-}

-- Circular, doubly-linked list based on the standard list type.
-- from https://wiki.haskell.org/Tying_the_Knot#Other_Examples
data DList a = DLNode (DList a) a (DList a)
mkDList :: (Show a) => [a] -> DList a
mkDList [] = error "must have at least one element"
mkDList xs = let (first,last) = go last xs first
             in first

go :: (Show a) => DList a -> [a] -> DList a -> (DList a, DList a)
go prev [] next = (next, prev)
go prev (x:xs) next = let this        = DLNode prev x rest
                          (rest,last) = go this xs next
                      in (this,last)
