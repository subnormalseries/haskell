module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)


roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' a = read (show a)


main = do
  print $ roundTrip 4
  print (id 4)
  print (roundTrip'' 4 :: Integer)

data Product a b = Product a b deriving (Eq, Show)

productUnpackAOnly :: Product a b -> a
productUnpackAOnly (Product x _) = x

productUnpackBOnly :: Product a b -> b
productUnpackBOnly (Product _ y) = y

productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)

data SumOfThree a b c =
    FirstPossible a
 |  SecondPossible b
 |  ThirdPossible c deriving (Eq, Show)


f :: SumOfThree a b c -> Integer
f (FirstPossible  _) = 0
f (SecondPossible _) = 1
f (ThirdPossible  _) = 2

-- This should error if the number is < 0
-- This results in an infinite loop
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)


factorial' :: Integer -> Integer
factorial' x | x < 0  = error "Cannot perform factorial of negative numbers"
             | x == 0 = 1
             | x > 1  = factorial' (x - 1)

inc :: Num a => a -> a
inc = (+ 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0     n = n
incTimes times n = inc (incTimes (times - 1) n)



applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0     f b = b
applyTimes times f b = f (applyTimes (times - 1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+ 1) n

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0     f b = b
applyTimes' times f b = f . applyTimes' (times - 1) f $ b

g :: Bool -> Maybe Int
g False = Just 0
g _     = Nothing

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

-- Integral Division
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy numerator denominator = go numerator denominator 0
 where
  go n d count | d == 0         = error "Cannot divide by 0"
               | n < 0 && d > 0 = go ((negate n) - d) d (count + 1)
               | n < d          = (count, n)
               | otherwise      = go (n - d) d (count + 1)

func :: [a] -> [a] -> [a]
func x y = x ++ y

-- dividedBy is a partial function as it doesn't cater for negative numbers
-- 10 + (-2), 8
-- 8  + (-2), 6
-- 6   + (-2), 4
-- 4   + (-2), 2
-- 2   + (-2), 0

-- McCarthy91 function
mc91 :: Integer -> Integer
mc91 n = case (n > 100) of
  True  -> n - 10
  False -> mc91 (mc91 (n + 11))

mc91' :: Integer -> Integer
mc91' n | n > 100  = n - 10
        | n <= 100 = mc91' . mc91' $ (n + 11)


