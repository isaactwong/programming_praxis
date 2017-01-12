import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Applicative

{-
12 August 2011

Daniel Tunkelang posted this interview question to his blog:

Given an input string and a dictionary of words, segment the input string into a space-separated sequence of dictionary words if possible. For example, if the input string is “applepie” and dictionary contains a standard set of English words, then we would return the string “apple pie” as output.

He also gave a number of constraints: The dictionary provides a single operation, exact string lookup, and is a given to the task; you are not to consider how to implement the dictionary, nor or you to worry about stemming, spelling correction, or other aspects of the dictionary. The output may have more than two words, if there is more than one solution you only need to return one of them, and your function should indicate if there are no solutions.
-}

-- this doesn't work...yet. straighten it out with better idiomatic monad chaining. and the types are messed.
word_breaks :: String -> Maybe String
word_breaks "" = Nothing
word_breaks s = if Map.member s dict == True
                   then Just s
                   else do
                             x <- Data.List.map ((flip Map.lookup) dict) (inits s)
                             y <- word_breaks $ drop (length x) 
                             (if x == Nothing || y == Nothing then Nothing else Just x ++ " " ++ y)

dict :: Map.Map String String
dict = Map.fromList [("a","a"), ("aa","aa"), ("aaa","aaa"), ("ab","ab"), ("apple","apple"), ("apricot","apricot"), ("is","is"), ("pie","pie"), ("test","test"), ("this","this")]

{-
                else answer $ mapM (\x -> Just ((x++" ") ++) <*> (word_breaks (drop_prefix x s))) ns
                where ns = catMaybes $ Data.List.map ((flip Map.lookup) dict) (inits s)
                      drop_prefix x s = drop (length x) s

answer :: [String] -> Maybe String
answer [] = Nothing
answer xs = return (head xs)
-}