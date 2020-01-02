sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple :: Num a => a -> a
triple x = 3 * x


triple' = (*3)


waxOn = x * 5
        where 
            y = z + 8
            x = y ^ 2
            z = 7


waxOff = triple

half :: Fractional a => a -> a
half x = x / 2

square x = x * x

squareAndMultiplyPi x = pi * (x * x)

perimeter x y = (x * 2) + (y * 2)
perimeter' x y = x * 2 + y * 2

f x = x / 2 + 9
f' x = x / (2 + 9)
