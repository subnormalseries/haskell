module Arith2 where

-- Point free way of writing an add function
add :: Int -> Int -> Int
add = (+)

addOne' :: Int -> Int
addOne' = (+ 1)

addOne :: Int -> Int
addOne = \x -> x + 1

main :: IO ()
main = do
  print (0)

tensDigit :: (Integral a) => a -> a
tensDigit x = d
 where
  xLast = x `div` 10
  d     = xLast `mod` 10

-- tensDigit' :: (Integral a) => a -> a
-- tensDigit' x = (mod 10) . fst . divMod x $ 10


tensDigit' x = mod (fst . divMod x $ 10) 10
hundredsDigit x = mod (fst . divMod x $ 100) 100

nthDigit' x y = mod f y where f = fst . divMod x $ y

-- Case statement
foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
  True  -> x
  False -> y

-- Guard clauses
foldBool' :: a -> a -> Bool -> a
foldBool' x y z | z         = x
                | otherwise = y


-- if then else
foldBool'' :: a -> a -> Bool -> a
foldBool'' x y z = if z then x else y


-- Pattern matching
foldBool''' :: a -> a -> Bool -> a
foldBool''' x _ True  = x
foldBool''' _ y False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


