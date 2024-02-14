module StandardFunctions where
    myAnd :: [Bool] -> Bool
    myAnd [] = True
    myAnd (x:xs) = if x == False then False else myAnd xs

    myAnd2 :: [Bool] -> Bool
    myAnd2 [] = True
    myAnd2 (x:xs) = x && myAnd xs

    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (x:xs) = x || myOr xs

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f [] = False
    myAny f (x:xs) = (f x) || myAny f xs

    myElem :: Eq a => a -> [a] -> Bool
    myElem y [] = False
    myElem y (x:xs) = y == x || myElem y xs

    myElem2 :: Eq a => a -> [a] -> Bool
    myElem2 y x = any (y==) x

    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = (myReverse xs) ++ [x]

    squish :: [[a]] -> [a]
    squish [] = []
    squish (x:xs) = x ++ (squish xs)

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f [] = []
    squishMap f (x:xs) = (f x) ++ (squishMap f xs)

    squishAgain :: [[a]] -> [a]
    squishAgain y = (squishMap (\x -> x) y)

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f [x] = x
    myMaximumBy f (x:y:xs)
        | (f x y) == GT = myMaximumBy f (x:xs)
        | otherwise = myMaximumBy f (y:xs)

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy f [x] = x
    myMinimumBy f (x:y:xs)
       | (f x y) == LT = myMinimumBy f (x:xs)
       | otherwise = myMinimumBy f (y:xs)

    myMaximum :: (Ord a) => [a] -> a
    myMaximum x = myMaximumBy compare x
    
    myMinimum :: (Ord a) => [a] -> a
    myMinimum x = myMinimumBy compare x