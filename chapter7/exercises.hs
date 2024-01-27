f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f (a,_,c) (d,_,f) = ((a,d), (c,f))

funcZ x =
    case x + 1 == 1 of
        True -> "AWESOME"
        False -> "wut."

pal xs =
    case xs == reverse xs of
        True -> "yes"
        False -> "no"

pal' xs =
    case y of
        True -> "yes"
        False -> "no"
    where y = xs == reverse xs

functionC :: (Eq a, Ord a) => a -> a -> a
functionC x y =
    case x > y of True -> x
                  False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
    case (even n) of True -> n + 2
                     False -> n