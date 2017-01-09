import Data.List

{-
2 September 2011

These two problems seem to be on every list of programming interview questions:

1) Remove all duplicate characters from a string. Thus, “aaabbb” becomes “ab” and “abcbd” becomes “abcd”.

2) Replace all runs of consecutive spaces with a single space. Thus, “a.b” is unchanged and “a..b” becomes “a.b”, using a dot to make the space visible.

Your task is to write the two requested functions. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.
-}

remove_dups :: String -> String
remove_dups xs = foldl (\acc x -> if elem x acc then acc else acc ++ [x]) "" xs

trim_spaces :: String -> String
trim_spaces xs = concatMap (\x -> if (head x == ' ') then " " else x) (group xs)