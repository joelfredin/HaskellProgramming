module Jamine where

{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import Data.Typeable
import Data.Int
import Data.List
import Data.Maybe (isJust)

data Price =
   Price Integer deriving (Eq, Show)

data Size =
   Size Integer deriving (Eq, Show)

data Manufacturer =
   Mini
 | Mazda
 | Tata
   deriving (Eq, Show)

data Airline =
   PapuAir
 | CatapultsR'Us
 | TakeYourChancesUnited
   deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 1000)

data Tree = Leaf Float | Tree deriving (Typeable)
isTree :: (Typeable a) => a -> Bool
isTree x = isJust (cast x :: Maybe Tree)

{-
isCar :: Vehicle -> Bool
isCar x = if if x == Car then True else False
-}

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _       = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars (x:xs) = [isCar x] ++ (areCars xs)

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

-- nullary
data Example0 =
   Example0 deriving (Eq, Show)

-- unary
data Example1 =
   Example1 Int deriving (Eq, Show)

-- product of Int and String
data Example2 =
   Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)

data Example = MakeExample deriving (Eq, Show)

data Example3 = MakeExample3 Int deriving (Eq, Show)

data Goats = Goats Int deriving (Eq, Show)

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Cows =
   Cows Int deriving (Eq, Show)

tooManyNewGoats :: NewGoats -> Bool
tooManyNewGoats (NewGoats n) = n > 42

class TooMany a where
   tooMany :: a -> Bool

instance TooMany Int where
   tooMany n = n > 42

instance TooMany (Int, String) where
   tooMany (a,b) = a > 11 && b == "HEllo"

instance TooMany (Int, Int) where
   tooMany (a,b) = (a + b) > 42

--- Come back to exercise 11.8.3
{- 
instance TooMany :: (Num a, TooMany a) => (a, a) where
    tooMany (x, y) = (x + y) > 11
-}
newtype NewGoats =
   NewGoats Int deriving (Eq, Show, TooMany)

{-

instance TooMany NewGoats where
   tooMany (NewGoats n) = tooMany n

-}

data NumberOrBool =
   Numba Int8
 | BoolyBool Bool
 deriving (Eq, Show)

data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

data TwoQs =
 MkTwoQs QuantumBool QuantumBool
 deriving (Eq, Show)

type TwoQs2 = (QuantumBool, QuantumBool)

data Person = MkPerson String Int deriving (Eq, Show)

nameae :: Person -> String
nameae (MkPerson s _) = s

data Person2 =
   Person2 { name :: String, age :: Int} deriving (Eq, Show)

jm = Person2 "Julie" 22
ca = Person2 "Chris" 31

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  Jam Fruit Int
  deriving (Eq, Show, Ord)

data JamJars2 =
  JamJars2 {fruit :: Fruit
           , jars :: Int}
           deriving (Eq, Show, Ord)

row1 = JamJars2 Peach 3
row2 = JamJars2 Plum 4
row3 = JamJars2 Apple 5
row4 = JamJars2 Blackberry 6
row5 = JamJars2 Plum 7
row6 = JamJars2 Apple 8
row7 = JamJars2 Apple 9
row8 = JamJars2 Apple 3
allJam = [row1, row2, row3, row4, row5, row6, row7, row8]
allJam2 = [row1, row2, row4, row7]

totalJarofJams :: [JamJars2] -> Int
totalJarofJams [] = 0
totalJarofJams (x:xs) = (jars x) + (totalJarofJams xs)

mostRow :: [JamJars2] -> JamJars2
---mostRow [] = JamJars2 Peach 0
mostRow (x:y:[]) = if x > y then x else y
mostRow (x:y:xs) = if (x > y) then (mostRow (x:xs)) else (mostRow (y:xs))

sortGT :: JamJars2 -> JamJars2 -> Ordering
sortGT x y = compare (jars x) (jars y)

sortGT2 :: JamJars2 -> JamJars2 -> Ordering
sortGT2 x y = compare (fruit x) (fruit y)

groupBool :: JamJars2 -> JamJars2 -> Bool
groupBool x y = (fruit x) == (fruit y)

count_number = sortBy sortGT allJam
order_fruit = groupBy groupBool (sortBy sortGT2 allJam)