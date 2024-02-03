returnListOfStrings :: String -> [String]
returnListOfStrings x
    | dropWhile (== ' ') (dropWhile (/= ' ') x) == "" = [x]
    | otherwise = [(takeWhile (/= ' ') x)] ++ (returnListOfStrings (dropWhile (== ' ') (dropWhile (/= ' ') x)))
