module Recusion where

--first num is numTimes to increment
--second number is the number we are starting at
-- so incTimes 5 10 = 15
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = (+1) (incTimes (times - 1) n)

-- Takes a num, function to apply and the starting point
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ x = x
applyTimes n f x = (f . applyTimes (n - 1) f) $ x


f :: Bool -> Maybe Int
f False = Just 0
f True = Nothing


fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)


dividedBy :: Integer -> Integer -> (DividedResult, Integer) 
dividedBy numerator denominator = go numerator denominator 0
    where 
        go n d count
            | d == 0 = (DividedByZero, 0)
            | n < 0 && d > 0 = go (n + d) d (count - 1)
            | n < 0 && d < 0 = go (n - d) d (count + 1)
            | n > 0 && d < 0 = go (n + d) d (count - 1)
            | n < d = (Result count, n)
            | n > d  && d < 0 = (Result count, n)
            | otherwise = go (n - d) d (count + 1) 

sum' :: (Integral a) => a -> a
sum' 0 = 0
sum' x = x + sum' (x - 1) 

product' :: (Integral a) => a -> a -> a
product' _ 0 = 0
product' x n = x + (product' x (n - 1))


data DividedResult = Result Integer | DividedByZero deriving Show


-- McCarthy 91
mc91 :: Integer -> Integer
mc91 n
        | n >= 100 = n - 10
        | otherwise = 91


-- Numbers into words
