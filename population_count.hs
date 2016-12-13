-- 28Jan2011
-- The population count of a bitstring is the number of set bits (1-bits) in the string. For instance, the population count of the number 23, which is represented in binary as 10111, is 4. The population count is used in cryptography and error-correcting codes, among other topics in computer science; some people use it as an interview question. The population count is also known as Hamming weight.

-- Your task is to write a function that determines the population count of a number representing a bitstring

population_count :: Int -> Int
population_count = foldr (\x sum -> if x == '1' then sum + 1 else sum) 0 . to_binary_string

to_binary_string :: Int -> String
to_binary_string n
                 | n == 1 = "1"
                 | otherwise = (to_binary_string q) ++ (show r) where (q, r) = divMod n 2

