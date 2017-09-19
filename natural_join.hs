{-
15 June 2010

In relational databases, the natural join of two tables is an operator that takes two tables as input and returns a new table that combines the two input tables on their common keys. If the input tables are sorted, the join simply scans the two tables, writing records for the cross-product of all records with equal keys. For instance, the join of the two tables

Key  Field1  Field2               Key  Field3
 A      w       p                  A      1
 B      x       q        and       A      2
 B      y       r                  B      3
 C      z       s

is the table

Key  Field1  Field2  Field3
 A      w       p       1
 A      w       p       2
 B      x       q       3
 B      y       r       3

We represent a table as a file; each line is a record, and fields are separated by tabs. For simplicity, we’ll assume that the first field in each record is the key.

Your task is to write a program that takes two input files representing tables (you may assume they are sorted) and produces their natural join as output.
-}

import System.IO

readTable :: String -> IO [[String]]
readTable fileName = (readFile fileName) >>= (return . lines) >>= (return . map  words)
join :: [[String]] -> [[String]] -> [[String]]
join x y = [t1 ++ (tail t2) | t1 <- a, t2 <- b, (t1 !! 0) == (t2 !! 0)]
  where a = tail x
        b = tail y

main :: IO ()
main = do
  table1   <- readTable "table1.txt"
  table2   <- readTable "table2.txt"
  writeFile "output.txt" (show (join table1 table2))
