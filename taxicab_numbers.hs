{-

9 November 2012

We haven’t done a coding interview question for a while. Here’s one that is supposedly asked at Google:

The mathematician G. H. Hardy was on his way to visit his collaborator Srinivasa Ramanujan who was in the hospital. Hardy remarked to Ramanujan that he traveled in a taxi cab with license plate 1729, which seemed a dull number. To this, Ramanujan replied that 1729 was a very interesting number — it was the smallest number expressible as the sum of cubes of two numbers in two different ways. Indeed, 10^3 + 9^3 = 12^3 + 1^3 = 1729.

Given an arbitrary positive integer, how would you determine if it can be expressed as a sum of two cubes?

Your task is to write a function that returns all the ways a number can be written as the sum of two non-negative cubes; use it to verify the truth of Ramanujan’s statement.
-}

cubes :: Int -> [(Int, Int)]
cubes n = [(a,b) | a <- [0..s], b <- [0..s], a<=b, a^3 + b^3 == n]
      where 
            s = round $ (fromIntegral n)**(1/3)

taxicabs :: [(Int, [(Int,Int)])]
taxicabs = [(n, cubes n) | n <- [1..], length (cubes n) > 1]

-- Nice one from Bonzai Code
-- https://bonsaicode.wordpress.com/2012/11/09/programming-praxis-taxicab-numbers/
cube_sums :: Integer -> [(Integer, Integer)]
cube_sums n = f 0 r
          where
                r = round $ fromIntegral n**(1/3)
                f x y = if y < x
                        then []
                        else case compare (x^3 + y^3) n of
                             EQ -> (x,y) : (f (x+1) (y-1))
                             LT -> f(x+1) y
                             GT -> f x (y-1)

taxicabs' :: [(Integer, [(Integer,Integer)])]
taxicabs' = [(n, cube_sums n) | n <- [1..], length (cube_sums n) > 1]