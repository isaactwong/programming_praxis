import Data.List

-- 23Nov2010: String Subsets
-- Given two strings, determine if all the characters in the second string appear in the first string; thus, DA is a subset of ABCD. Counts matter, so DAD is not a subset of ABCD, since there are two D in the second string but only one D in the first string. You may assume that the second string is no longer than the first string.
-- Your task is to write a function to determine if one string is a subset of another string

{-
stringSubset :: String -> String -> Bool
stringSubset l r
             | (length l) > (length r) = False
             | otherwise = foldr (\x b -> if (elem x r) then (True && b) else False) True l
-}

-- Fancy ones from Bonsai Code
subset1 :: (Ord a) => [a] -> [a] -> Bool
subset1 xs ys = all (\(c,n) -> maybe False (n <=) (lookup c $ listCount ys)) $ listCount xs
                where listCount ls = map (\x -> (head x, length x)) . group . sort $ ls