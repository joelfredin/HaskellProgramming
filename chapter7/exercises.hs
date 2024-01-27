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

nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

dodgy :: (Show a, Num a) => a -> a -> a
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
   | y >= 0.7   = 'C'
   | y >= 0.9   = 'A'
   | y >= 0.8   = 'B'
   | y >= 0.59  = 'D'
   | y < 0.59   = 'F'
   where y = x / 100

pal2 :: (Eq a) => [a] -> Bool
pal2 xs
  | xs == reverse xs = True
  | otherwise        = False

numbers :: (Num a, Eq a, Ord a) => a -> a
numbers x
  | x < 0   = -1
  | x == 0  = 0
  | x > 0   = 1

myfunc :: Ord a => a -> a -> Bool
myfunc a b = True

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `divMod` 10
        d     = snd xLast

tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10


husD :: Integral a => a -> a
husD x = d2
  where xLast = x `divMod` 100
        d2    = snd xLast

foldBool :: a -> a -> Bool -> a
foldBool x y True = x
foldBool x y False = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z == True   = x
  | z == False  = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y z =
    case z == True of True -> x
                      False -> y

myg :: (a -> b) -> (a,c) -> (b,c)
myg f (a,c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

--roundTrip3 :: (Show a, Read b) => (a -> b)
--roundTrip3 = (read :: a -> String) . (show)

main = do
    print (roundTrip 4)
    print (id 4)

    print (roundTrip2 4)
    print (roundTrip3 4)