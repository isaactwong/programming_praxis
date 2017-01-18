import Data.Char

{-

5 April 2013

Today’s exercise appears from time to time on beginning-programmer message boards:

Write a program that, given n, returns the last non-zero digit of n! (factorial). For instance, 7! = 1 * 2 * 3 * 4 * 5 * 6 * 7 = 5040, and its last non-zero digit is 4.

Your task is to write a program to find the last non-zero digit of a factorial.
-}

last_digit :: Int -> Int
last_digit n = digitToInt $ last $ filter (/='0') (show (fact n))

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)