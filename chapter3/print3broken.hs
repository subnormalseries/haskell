module Print3Broken where
import GHC.Float (word2Double)

printSecond :: String -> IO()
printSecond greeting = do
    putStrLn greeting


main :: IO()
main = do
    putStrLn greeting
    printSecond greeting
    where greeting = "Yarrrrr"


thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex n = (!!) word (n-1)
    where word = "Curry is awesome"

rvrs :: String
rvrs = drop 9 word ++ take 4 (drop 5 word) ++ take 5 word
    where word = "Curry is awesome"