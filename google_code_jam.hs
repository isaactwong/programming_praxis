import Data.List

-- 15 Feb 2016

-- Store Credit: You receive a credit C at a local store and would like to buy two items. You first walk through the store and create a list L of all available items. From this list you would like to buy two items that add up to the entire value of the credit. The solution you provide will consist of the two integers indicating the positions of the items in your list (smaller number first). For instance, with C=100 and L={5,75,25} the solution is 2,3; with C=200 and L={150,24,79,50,88,345,3} the solution is 1,4; and with C=8 and L={2,1,9,4,4,56,90,3} the solution is 4,5.
store_credit :: Int -> [Int] -> (Int, Int)
store_credit c xs = head [(i,j) | (i,a) <- (zip [1..] xs), (j,b) <- (drop i (zip [1..] xs)), a + b == c]

-- Reverse Words: Given a list of space separated words, reverse the order of the words. Each input string contains L letters and W words. An input string will only consist of letters and space characters. There will be exactly one space character between each pair of consecutive words. For instance, the reverse of “this is a test” is “test a is this”, the reverse of “foobar” is “foobar”, and the reverse of “all your base” is “base your all”.
reverse_words  :: String -> String
reverse_words = unwords . reverse . words 

-- T9 Spelling: The Latin alphabet contains 26 characters and telephones only have ten digits on the keypad. We would like to make it easier to write a message to your friend using a sequence of keypresses to indicate the desired characters. The letters are mapped onto the digits as 2=ABC, 3=DEF, 4=GHI, 5=JKL, 6=MNO, 7=PQRS, 8=TUV, 9=WXYZ. To insert the character B for instance, the program would press 22. In order to insert two characters in sequence from the same key, the user must pause before pressing the key a second time. The space character should be printed to indicate a pause. For example “2 2” indicates AA whereas “22” indicates B. Each message will consist of only lowercase characters a-z and space characters. Pressing zero emits a space. For instance, the message “hi” is encoded as “44 444”, “yes” is encoded as “999337777”, “foo  bar” (note two spaces) is encoded as “333666 6660022 2777”, and “hello world” is encoded as “4433555 555666096667775553”.
-- Super clever solution from Bonsai Code.

encode :: String -> String
encode s = unwords =<< groupBy(\(a:_) (b:_) -> a == b && a > '0') (map code_term s)
       where code_term c = maybe "0" id (lookup c key)
             key = concat $ zipWith zip
                   (words "abc def ghi jkl mno pqrs tuc wxyz")
                   [[replicate n d | n <- [1..]] | d <- ['2'..]]
