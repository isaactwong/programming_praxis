import Data.List

{-
19 August 2011
Write a function that takes an input string and returns the first character from the string that is not repeated later in the string. For instance, the two letters “d” and “f” in the input string “aabcbcdeef” are non-repeating, and the function should return “d” since “f” appears later in the string. The function should return some indication if there are no non-repeating characters in the string.
-}

first_non_repeat :: (Eq a) => [a] -> Maybe a
first_non_repeat [] = Nothing
first_non_repeat (x:xs) = if elem x xs then first_non_repeat (filter_out x xs) else Just x
                 where filter_out x xs = filter (/=x) xs

