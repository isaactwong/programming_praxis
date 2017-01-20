import Data.List

{-
11 June 2013

Today’s exercise comes from our unending list of interview questions:

Find the longest run of consecutive characters in a string that contains only two unique characters; if there is a tie, return the rightmost. For instance, given input string abcabcabcbcbc, the longest run of two characters is the 6-character run of bcbcbc that ends the string.

Your task is to write the requested program.
-}

longest_substring :: String -> Maybe String
longest_substring s = if (length longest) == 0 then Nothing
                      else Just (last longest)
                  where
                        longest = sortBy sb (filter (\xs -> (length (nub xs)==2)) $ decompose s)
                        sb l r = compare (length l) (length r)

decompose :: String -> [String]
decompose [] = []
decompose xs@(x:xs') = (substrings xs) ++ (decompose xs')

substrings :: String -> [String]
substrings [] = []
substrings (x:xs) = [[x]] ++ map ([x]++) (substrings xs) 