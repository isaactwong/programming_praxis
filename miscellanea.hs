import Data.Numbers.Primes

{-
26 April 2011

FizzBuzz: Looking back over past exercises, I was surprised to find that we haven’t done this classic interview question. You are to write a function that displays the numbers from 1 to an input parameter n, one per line, except that if the current number is divisible by 3 the function should write “Fizz” instead of the number, if the current number is divisible by 5 the function should write “Buzz” instead of the number, and if the current number is divisible by both 3 and 5 the function should write “FizzBuzz” instead of the number. For instance, if n is 20, the program should write 1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, FizzBuzz, 16, 17, Fizz, 19, and Buzz on twenty successive lines.

Prime Words: Consider that a word consisting of digits and the letters A through Z can represent an integer in base 36, where the digits represent their base-10 counterparts, A is a decimal 10, B is a decimal 11, and so on, until Z is a decimal 35. For instance, PRAXIS36 = P36 × 365 + R36 × 364 + A36 × 363 + X36 × 362 + I36 × 361 + S36 × 360 = 25 × 365 + 27 × 364 + 10 × 363 + 33 × 362 + 18 × 361 + 28 × 360 = 25 × 60466176 + 27 × 1679616 + 10 × 46656 + 33 × 1296 + 18 × 36 + 28 × 1 = 1557514036. You are to write a function that takes a base-36 number as input and returns true if the number is prime and false if the number is composite.

Split A List: You are to write a function that takes an input list and returns two lists, the first half of the input list and the second half of the input list. If the input list has an odd number of elements, it is your choice in which half to place the center element. You are only permitted to scan the list once.

-}

-- FizzBuzz
fizzbuzz :: Int -> IO ()
fizzbuzz n = mapM_ (print . f) [1..n]
         where f n | mod n 15 == 0 = "FizzBuzz"
                   | mod n 5  == 0 = "Buzz"
                   | mod n 3  == 0 = "Fizz"
                   | otherwise     = (show n)

-- Prime Words
is_prime_word :: String -> Bool
is_prime_word w = isPrime . to_base_10 w

to_base_10 :: String -> Int
to_base_10 n = sum (zipWith (*) (map ((^)36) [0..]) (translate n))

translate :: String -> [Int]
translate = reverse . map f 
            where keys = zip (['0'..'9'] ++ ['A'..'Z']) [0..]
                  f x = maybe 0 id (lookup x keys)

-- Split A List
splits :: [a] -> ([a],[a])
splits xs = (take m xs, drop m xs)
       where m = div (length xs) 2