{-
27 July 2012

Today’s exercise continues our occasional series of exercises based on interview questions:

Design a data structure that provides push and pop operations, like a stack, plus a third operation that finds the minimum element. All three operations must perform in constant time. You may assume that all elements are distinct.

Your task is to write the three indicated functions.
-}

data Stack a = Stack ([a],[a]) deriving (Show)

push :: (Ord a) => a -> Stack a -> Stack a
push y (Stack ([],[]))  = Stack ([y],[y])
push y (Stack (xs, ms)) = if y < (head ms) then Stack (y:xs, y:ms) else Stack (y:xs,ms)

pop :: (Eq a) => Stack a -> (Maybe a, Stack a)
pop (Stack ([],[])) = (Nothing, Stack ([],[]))
pop (Stack (xs,ms))
    | (head ms) == (head xs) = (Just (head xs), Stack (tail xs, tail ms))
    | otherwise              = (Just (head xs), Stack (tail xs, ms))

stack_min :: Stack a -> (Maybe a, Stack a)
stack_min (Stack ([],[])) = (Nothing, Stack ([],[]))
stack_min (Stack (xs,ms)) = (Just (head ms), Stack (xs,ms))