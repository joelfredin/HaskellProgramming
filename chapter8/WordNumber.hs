module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 1 = "One"
  | n == 2 = "Two"
  | n == 3 = "Three"
  | n == 4 = "Four"
  | n == 5 = "Five"
  | n == 6 = "Six"
  | n == 7 = "Seven"
  | n == 8 = "Eight"
  | n == 9 = "Nine"


digits :: Int -> [Int]
digits n = go n 1
  where go n   count
         | count == 1 = [] ++ [(mod (div n count) 10)] ++ (go n (count * 10))
         | (div n count) < 10 = [(mod (div n count) 10)]
         | otherwise = [] ++ [(mod (div n count) 10)] ++ (go n (count * 10))

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . reverse . map digitToWord . digits