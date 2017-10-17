import Data.List

{-
13 October 2017

This interesting little question comes from Career Cup:

Given an array that contains only the elements -1 and 1, find the number of sub-arrays with a sum of zero. For instance, given the array [-1, 1, -1, 1], there are four sub-arrays that sum to zero: [-1, 1], [1, -1], [-1, 1] and [-1, 1, -1, 1].

Your task is to count the sub-arrays of a -1/1 array that sum to zero.
-}

allSubArrays :: [a] -> [[a]]
allSubArrays = concatMap (tail . inits) . tails

zeroSumSubArrays :: [Integer] -> [[Integer]]
zeroSumSubArrays = filter ((==0) . sum) . allSubArrays

