import Data.Map as M
import Data.List

{-

30 April 2013

We have today another exercise from our never-ending supply of interview questions:

Given a string, find the first character that appears only once in the string. For instance, given the string “aabbcddd”, the first character that appears only once is the “c” found at the 4th character in the string, counting from 0. Be sure that your program properly handles the case that all characters appear more than once.

Your task is to write a program that finds the first unrepeated character in a string, and its index in the string.
-}

first_unrepeated :: String -> Maybe (Int,Char)

first_unrepeated s = if length singletons == 0 then Nothing
                     else Just (head singletons)
                     where
                        singletons = sort . concat . elems $ (M.filter (\p -> (length p)==1) m)
                        m = Prelude.foldr (\(i,c) d -> M.insertWith (++) c [(i,c)] d) (M.fromList []) si
                        si = zip [0..] s