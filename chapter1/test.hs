sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")


triple x = 3 * x

half x = x / 2

square x = x * x

squareAndMultiplyPi x = pi * (x * x)

perimeter x y = (x * 2) + (y * 2)
perimeter' x y = x * 2 + y * 2

f x = x / 2 + 9
f' x = x / (2 + 9)
