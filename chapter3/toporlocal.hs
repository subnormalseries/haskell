module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x =
    x + woot + topLevelValue
    where woot = 10 :: Integer

topLevelValue ::Integer
topLevelValue = 5

area :: Floating a => a -> a
area d = pi * (r * r)
    where r = d /2