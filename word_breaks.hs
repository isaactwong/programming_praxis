import qualified Data.Map as Map
import Data.List

{-
12 August 2011

Daniel Tunkelang posted this interview question to his blog:

Given an input string and a dictionary of words, segment the input string into a space-separated sequence of dictionary words if possible. For example, if the input string is “applepie” and dictionary contains a standard set of English words, then we would return the string “apple pie” as output.

He also gave a number of constraints: The dictionary provides a single operation, exact string lookup, and is a given to the task; you are not to consider how to implement the dictionary, nor or you to worry about stemming, spelling correction, or other aspects of the dictionary. The output may have more than two words, if there is more than one solution you only need to return one of them, and your function should indicate if there are no solutions.
-}

-- Map containing the dictionary we are using to define word breaks.
dict :: Map.Map String String
dict = Map.fromList [("a","a"), ("aa","aa"), ("aaa","aaa"), ("ab","ab"), ("apple","apple"), ("apricot","apricot"), ("is","is"), ("pie","pie"), ("test","test"), ("this","this")]

guard :: Bool -> [String]
guard True = [""]
guard False = []

suffix :: String -> String -> String
suffix x y = drop (length x) y

candidateWords :: String -> [String]
candidateWords s = (tail . inits) s

wordBreaks :: String -> [String]
wordBreaks "" = []
wordBreaks s
  | Map.member s dict == True = [s]
  | otherwise = do
      x <- candidateWords s
      _ <- guard (Map.member x dict)
      y <- wordBreaks (suffix x s)
      return (x++" "++y)
