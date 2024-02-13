{-

TODO: Think about unCeasarShift, what values to use in the pattern-matching when value is less than 97.

-}

module Cipher where
    import Data.Char
    
    ceasarShift :: String -> Int -> String
    ceasarShift "" n = ""
    ceasarShift (x:xs) n
      | (ord x) + n > 122 = [(chr (71 + (mod ((ord x) + n) 97)))] ++ (ceasarShift xs n)
      | otherwise = [(chr ((ord x) + n))] ++ (ceasarShift xs n)

    unCeasarShift :: String -> Int -> String
    ceasarShift "" n = ""
    ceasarShift (x:xs) n
      | (ord x) - n < 97 = [(chr (71 + (mod ((ord x) + n) 97)))] ++ (ceasarShift xs n)
      | otherwise = [(chr ((ord x) + n))] ++ (ceasarShift xs n)