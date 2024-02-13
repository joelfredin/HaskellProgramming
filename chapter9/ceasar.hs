module Cipher where
    import Data.Char
    
    ceasarShift :: String -> Int -> String
    ceasarShift "" n = ""
    ceasarShift (x:xs) n
      | (ord x) + n > 122 = [(chr (71 + (mod ((ord x) + n) 97)))] ++ (ceasarShift xs n)
      | otherwise = [(chr ((ord x) + n))] ++ (ceasarShift xs n)
    
{-= [(chr (71 + (mod ((ord x) + 1) 123)))] ++ (ceasarShift xs)-}