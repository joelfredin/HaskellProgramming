module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen 

myLines :: String -> [String]
myLines x
    | dropWhile (== '\n') (dropWhile (/= '\n') x) == "" = [x]
    | otherwise = [(takeWhile (/= '\n') x)] ++ (myLines (dropWhile (== '\n') (dropWhile (/= '\n') x)))

shouldEqual =
 [ "Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

parameterizedLines :: String -> Char -> [String]
parameterizedLines x y
    | dropWhile (== y) (dropWhile (/= y) x) == "" = [x]
    | otherwise = [(takeWhile (/= y) x)] ++ (parameterizedLines (dropWhile (== y) (dropWhile (/= y) x)) y)

main :: IO ()
main =
 print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)