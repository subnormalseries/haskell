module Print3Flipped where

myGreeting :: String 
myGreeting = (++) "hello " "world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = (++) hello $ " " ++ world  