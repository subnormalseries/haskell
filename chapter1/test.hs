sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple :: Num a => a -> a
triple x = 3 * x


triple' :: Integer -> Integer
triple' = (*3)


waxOn :: Integer
waxOn = x * 5
        where
            y = z + 8
            x = y ^ 2
            z = 7


waxOff :: Integer -> Integer
waxOff = triple

half :: Fractional a => a -> a
half x = x / 2

square :: Num a => a -> a
square x = x * x

squareAndMultiplyPi :: Floating a => a -> a
squareAndMultiplyPi x = pi * (x * x)

perimeter :: Num a => a -> a -> a
perimeter x y = (x * 2) + (y * 2)

perimeter' :: Num a => a -> a -> a
perimeter' x y = x * 2 + y * 2


f :: Fractional a => a -> a
f x = x / 2 + 9


f' :: Fractional a => a -> a
f' x = x / (2 + 9)


bools :: [Bool]
bools = [True, False, True]

nums ::[[Int]]
nums = [[1,2,3], [4,5,6]]

add' :: Int -> Int -> Int
add' x y = x + y


copy' :: a -> (a, a)
copy' x = (x, x)

apply :: (a -> b) -> a -> b
apply g = g



second :: [a] -> a
second xs = head (tail xs)


swap :: (a, b) -> (b, a)
swap (x, y) =  (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)


double'' :: Num a => a -> a
double'' x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs


twice :: (t -> t) -> t -> t
twice g x = g (g x)








double :: Num a => a -> a
double x = x + x

sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x:xs) = foldr (+) x xs

{-
sum' [x] = x + sum' [] = x + 0 = x
-}

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = foldr (*) x xs






































