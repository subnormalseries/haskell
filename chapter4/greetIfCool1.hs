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