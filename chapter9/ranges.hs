eftInt :: Int -> Int -> [Int]
eftInt x y
    | x == y = [y]
    | otherwise = [x] ++ (eftInt (succ x) y)

eftChar :: Char -> Char -> [Char]
eftChar a b
    | a == b = [b]
    | otherwise = [a] ++ (eftChar (succ a) b)