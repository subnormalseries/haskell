module Work where 

--anonymous functions
triple :: Integer -> Integer
triple = (*3)

--The same function but anonymous
a = (\x -> x * 3)
    
mTh x y z = x * y * z
mTh' x y = \z -> x * y * z
mTh'' x = \y -> \z -> x * y * z
mTh''' = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \m ->  m + 1

addFive x y = (if x > y then y else x) + 5
addFive' = \x -> \y -> (min x y) + 5


functionC x y = if (x > y) then x else y
functionC' x y = case (x > y) of
    True -> x
    False -> y

dodgy :: Num a => a -> a -> a
dodgy x y = x + (y * 10)
oneIsDodgy = dodgy 1


numbers x 
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1


f :: Int -> [Int] -> Int
f = foldr (+) 

f' = length . filter (== 'a')


tensDigit :: Integral a => a -> a
tensDigit x = d 
    where 
        xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' x = mod (fst (x `divMod` 10)) 10

hunsDigit x = d2
        where d2 = mod (fst (x `divMod` hundo)) hundo
              hundo = 100

foldBool :: a -> a -> Bool -> a
foldBool x y bool
        | bool = x
        | otherwise = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool = case bool of
    True -> x
    False -> y


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a , c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip x = (read(show x) :: Read b)  
