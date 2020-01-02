module Sing where
    
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow" 

sing = if (x < y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"


subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

subtractOne :: Integer -> Integer
subtractOne = subtractStuff 1


result = subtractOne 11


curry' f a b = f (a, b) 
uncurry f (a,b) = f a b
-- This is using uncurrying
test = curry' subtractStuff 1 10

--let answer = uncurriedSubtractStuff (1, 11)