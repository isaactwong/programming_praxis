import Data.List

-- 22Nov2016
-- Given two lists, find all the items in the first list that are not present in the second list. For instance, if (5 15 2 20 30 40 8 1) is the first list and (2 20 15 30 1 40 0 8) is the second list, the item 5 is present in the first list but not in the second list.

notIn :: (Eq a) => [a] -> [a] -> [a]
notIn xs ys = filter (flip notElem $ ys) xs

-- Cheap and easy with Set differencing from Haskell.
-- notIn xs ys = xs \\ ys