myFilter :: String -> [String]
myFilter x = filter (\y -> y /= "a" && y /= "an" && y /= "the") (words x)