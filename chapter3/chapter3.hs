module Chapter3 where

dropLastItem :: [a] -> [a]
dropLastItem y = take ((length y) - 1) y

dropLastLetter :: String -> String
dropLastLetter y = take (allButOne) y
    where allButOne = (length y) - 1

thirdLetter :: String -> Char
thirdLetter x = (!!) x 2

letterIndex :: Int -> Char
letterIndex x = phrase !! x 
    where phrase = "Curry is awesome!"

rvrs :: String
rvrs = (drop 9 phrase) ++ " " ++ (take 2 $ drop 6 phrase) ++ " " ++ (take 5 phrase) 
    where phrase = "Curry is awesome"



