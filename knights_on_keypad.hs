{-

20 January 2012

The numbers on a telephone keypad are arranged thus:

1 2 3
4 5 6
7 8 9
  0

Starting from the digit 1, and choosing successive digits as a knight moves in chess, determine how many different paths can be formed of length n. There is no need to make a list of the paths, only to count them.

A knight moves two steps either horizontally or vertically followed by one step in the perpendicular direction; thus, from the digit 1 on the keypad a knight can move to digits 6 or 8, and from the digit 4 on the keypad a knight can move to digits 3, 9 or 0. A path may visit the same digit more than once.

Your task is to write a function that determines the number of paths of length n that a knight can trace on a keyboard starting from digit 1.

-}

knight_paths :: Int -> Int
knight_paths n = length $ last $ take n $ iterate (>>= knightMove) [1]

knightMove :: Int -> [Int]
knightMove 0 = [4,6] 
knightMove 1 = [6,8]
knightMove 2 = [7,9]
knightMove 3 = [4,8]
knightMove 4 = [0,3,9]
knightMove 5 = []
knightMove 6 = [0,1,7]
knightMove 7 = [2,6]
knightMove 8 = [1,3]
knightMove 9 = [2,4]