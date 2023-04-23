module Print2 where

main :: IO()
main = do
    putStrLn "Count to Four for me:"
    putStr "one, two"
    putStr ", three, and "
    putStrLn "four!"