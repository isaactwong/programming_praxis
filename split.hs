{-
30 Dec 2011

Given a list, return the first half of the list as one list and the second half of the list as a second list. For instance, given the input list {1 2 3 4}, output the two lists {1 2} and {3 4}. If the input list has an odd number of items, the middle item can go to either list, so that the input list {1 2 3 4 5} can result in the output lists {1 2} and {3 4 5} or the output lists {1 2 3} and {4 5}.

-}

-- Maybe a little to easy in Haskell.
split :: [a] -> ([a],[a])
split xs = splitAt (div l 2) xs
      where l = length xs

-- A little harder in Haskell with the tortoise and the hare.
split' :: [a] -> ([a],[a])
split' xs = f (fst (foldl (\((l,r),end) (t,h) -> ((l ++ [t], [(last end)] ++ r), init end)) (([],[]),xs) ys))
       where eo = every_other xs
             ys = zip xs eo
             f (l,r) = if odd (length xs) then (l, tail r) else (l,r)

every_other :: [a] -> [a]
every_other [] = []
every_other [a] = [a]
every_other (x:y:xs) = x : (every_other xs)
