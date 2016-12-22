{-
18 March 2011

Print the numbers from 1 to 1000 without using any loop or conditional statements. Don’t just write the printf() or cout statement 1000 times.

Your task is to write the program in your favorite language. Have fun — think of as many solutions as you can, or the wackiest, or the fewest characters in the source file, or some other best-in-category. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.
-}

loopy :: Integer -> IO()
loopy n = mapM_ print [1..n]