module Print3Broken where

printSecond :: IO ()
printSecond = do putStrLn greeting

greeting :: String
greeting = "Yarrrrrrrrrrrr"

main :: IO ()
main = do 
    putStrLn greeting
    printSecond