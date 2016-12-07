-- A happy number is defined by the following process. Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers, while those that do not end in 1 are unhappy numbers (or sad numbers).
-- Your task is to write a function to identify the happy numbers less than a given limit
-- Solution help from comments in PP.

isHappy :: Int -> Bool
isHappy n = hapSeq [] n
        where
                hapSeq _ 1 = True
                hapSeq xs x = (notElem x xs) && (hapSeq (x : xs) (happyTransform x))

happyInRange :: Int -> [Int]
happyInRange bound = filter isHappy [1..bound]

happyTransform :: Int -> Int
happyTransform = sum . map (flip (^) 2) . digits

digits :: Int -> [Int]
digits = map (\x -> read [x] :: Int) . show 

-- Why can't I get this right fold to work?
happySeq :: Int -> [Int]
happySeq n = foldr (\x acc -> acc ++ [happyTransform (last acc)]) [n] (repeat 1)
