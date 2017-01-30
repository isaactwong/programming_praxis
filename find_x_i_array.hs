import Data.List

{-
26 July 2013

We have today another of our unlimited supply of interview questions: this one supposedly comes from Amazon:

Given an array X[0..n-1] of integers sorted into ascending order with no duplicates, find an array item that is also its index, so that X[i] = i.

For example, X[3] = 3 in the array shown below:

i     0 1 2 3 4 5
x[i] -3 0 1 3 5 7

-}

naive :: [Int] -> Maybe (Int)
naive xs = f (find (\(i,x) -> i==x) $ zip [0..] xs)
      where 
            f Nothing      = Nothing
            f (Just (i,x)) = Just i


fixed_point :: [Int] -> Maybe Int
fixed_point xs = binary_search p (0,(length xs)-1)
            where p m = compare m (xs !! m)
                  binary_search p (low,high)
                                | high < low          = Nothing
                                | (xs !! low)  > low  = Nothing
                                | (xs !! high) < high = Nothing
                                | otherwise  =
                                             let mp = div (low+high) 2 in
                                             case p mp of
                                                  LT -> binary_search p (low,mp-1)
                                                  GT -> binary_search p (mp+1,high)
                                                  EQ -> Just mp
                                                      