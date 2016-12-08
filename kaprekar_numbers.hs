-- 21 September 2010
-- Consider an n-digit number k. Square it and add the right n digits to the left n or n-1 digits. If the resultant sum is k, then k is called a Kaprekar number. For example, 9 is a Kaprekar number since 92 = 81 and 8 + 1 = 9 and 297 is a Kaprekar number since 297^2 = 88209 and 88 + 209 = 297.
-- Your task is to write a function that identifies Kaprekar numbers and to determine the Kaprekar numbers less than a thousand

isKaprekar :: Int -> Bool
isKaprekar k = k == sum
           where
                n = length . show $ k
                (left, right) = splitAt n  (reverse . show $ (k^2))
                sum = (read . reverse $ left) + (read . reverse $ right)

kaprekarNumber :: Int -> [Int]
kaprekarNumber n = filter isKaprekar [4..n]

