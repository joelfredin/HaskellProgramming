module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes2 :: (Eq a, Num a) => a -> a -> a
incTimes2 times n = applyTimes times (+1) n

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = (fibonacci (x -  1)) + (fibonacci (x -  2))

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = div

data DividedResult =
      Result Integer
    | DividedByZero

dividedBy2 :: Integral a => a -> a -> (a,a)
dividedBy2 num denom = go num denom 0
  where go n    d count
         | n <  d = (count, n)
         | otherwise = go (n - d) d (count + 1)

sumNumbers :: (Eq a, Num a) => a -> a
sumNumbers 0 = 0
sumNumbers n = n + (sumNumbers (n - 1))

multiplyTwoNum :: Integral a => a -> a -> a
multiplyTwoNum 1 y = y
multiplyTwoNum x y = y + (multiplyTwoNum (x - 1) y)

revertPoints :: (Integral a, Integral a) => (a,a) -> a -> a -> (a,a)
revertPoints (a,b) c d
    | c >= 0 && d >= 0 = (a,b)
    | c <= 0 && d >= 0 = (-a,b)
    | c >= 0 && d <= 0 = (a,-b)
    | c <= 0 && d <= 0 = (a,b)

dividedBy3 :: Integral a => a -> a -> (a,a)
dividedBy3 num denom
  | num <= 0 && denom <= 0 = revertPoints (dividedBy2 (-num) (-denom)) num denom
  | num >=0 && denom >= 0 = revertPoints (dividedBy2 num denom) num denom
  | num <= 0 && denom >= 0 = revertPoints (dividedBy2 (-num) denom) num denom
  | num >= 0 && denom <= 0 = revertPoints (dividedBy2 num (-denom)) num denom