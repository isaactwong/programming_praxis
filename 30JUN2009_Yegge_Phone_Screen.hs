import Data.List

-- Steve Yegge is a programmer and popular blogger. One of his blog entries proposes these seven phone-screen coding exercises:

-- Write a function to reverse a string.
reverseStr :: [a] -> [a]
reverseStr [] = []
reverseStr (x : xs) = (reverseStr xs) ++ [x]

-- Or, a fancy one
-- reverseStr = foldl (flip (:)) []

-- Write a function to compute the Nth fibonacci number.
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- Or, a fancy one
-- fib n = fibs !! n
--    where fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

-- Print out the grade-school multiplication table up to 12 x 12.
-- timesTable :: IO ()
-- timesTable = mapM putStrLn ["h", "i"]

-- Write a function that sums up integers from a text file, one per line.

-- Write a function to print the odd numbers from 1 to 99.
oddNumbers :: Int -> [Int]
oddNumbers n = foldr (\x acc -> if (mod x 2) == 1 then ([x] ++ acc) else acc) [] [1..n]
oddAgain n = filter odd [1..n]

-- Find the largest int value in an int array.
maxElem :: (Ord a) => [a] -> a
maxElem [x] = x
maxElem (x : xs) = max x (maxElem xs)

-- Format an RGB value (three 1-byte numbers) as a 6-digit hexadecimal string.
