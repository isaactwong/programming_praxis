{-

31 January 2012

Write a function that takes two input strings and determines if one is a rotation of the other. For instance, “ProgrammingPraxis” and “PraxisProgramming” are rotations of each other, but “ProgrammingPrasix” is not a rotation of “ProgrammingPraxis” because the last three letters are out of order.

-}

is_rotation :: String -> String -> Bool
is_rotation s t = elem s (take (length t) (iterate rotate t))

rotate :: String -> String
rotate [] = []
rotate xs = [last xs] ++ (init xs)

-- Clever rotation from stackoverflow
-- http://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs