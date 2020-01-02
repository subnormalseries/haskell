module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool
        then putStrLn "eyyyy. What's shakin'?"
    else
        putStrLn "pshhhhh."
    where cool = coolness == "downright frosty yo"



greetIfCool' :: String -> IO ()
greetIfCool' coolness =
    if cool coolness
        then putStrLn "eyyyy. What's shakin'?"
    else
        putStrLn "pshhhhh."
    where cool v = v == "downright frosty yo"


tuple :: (,) String Integer
--tuple :: (String, Integer)
tuple = (,) "Dog" 5

list :: [] String
--list :: [String]
list = "Dog" : []


list' :: [] String
list' = "5" : list


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ (x : []) 


isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = (reverse' x == x)


myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x
