import Data.List
import Data.Function 

-- 25Nov2016
-- Question 1
-- A kidnapper wants to write a ransom note by cutting characters from the text of a magazine. Given two strings containing the characters of the ransom note and the characters of the magazine, write a program to determine if the ransom note can be formed from the magazine.

isRansomNote :: String -> String -> Bool
isRansomNote r m = (intersect r m) == r

-- Question 2
-- Write a program that operates in linear time that finds the item in a list that appears the most times consecutively.
maxRunElem :: (Eq a) => [a] -> a
maxRunElem xs = head $ maximumBy (compare `on` length) (group xs)

-- Question 2
-- Given two finite streams of integers that are too large to fit in memory, write a program that finds the integers that appear in both streams; it must operate in time linear in the length of the longer of the two streams.

