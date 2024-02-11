import Data.Char
uppercaseStrings :: String -> String
uppercaseStrings "" = ""
uppercaseStrings (x:xs) = [(toUpper x)] ++ (uppercaseStrings xs)

capFirstLetter :: String -> Char
capFirstLetter = toUpper . head