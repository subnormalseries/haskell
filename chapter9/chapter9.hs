module ChapterNine where

import Data.Bool
import Data.Char
import Data.List

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_ : xs) = Just xs


myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x : _) = Just x

myEft :: Enum a => a -> a -> [a]
myEft = undefined 

myWords :: String -> [String]
myWords "" = []
myWords word = (takeWhile notSpace word):[] 
    ++ myWords (dropSpace word)
    where dropSpace = (dropWhile space . dropWhile notSpace)
          notSpace = (/= ' ')
          space = (== ' ')

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines sentence = takeWhile (/='\n') sentence :[] ++ 
                   myLines (dropWhile (== '\n') (dropWhile (/= '\n') sentence))

splitOnChar :: Char -> String -> [String]
splitOnChar _ "" = []
splitOnChar char input = takeWhile (/= char) input : [] ++
                             splitOnChar char (dropWhile (== char) (dropWhile ((/= char)) input))


myWords' = splitOnChar ' '
myLines' = splitOnChar '\n'


b :: [(,) Integer Integer]
b = [(x, y) | x <- [1..10], y <- [10..20], rem x 2 == 0]


mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
c = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]


blah = enumFromTo 'a' 'z'
work = take 1 [x^y | x <- [1..5], y <- [2, undefined]]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x :xs) = (f x : []) ++ myMap f xs

itIsMystery xs = map (\x -> elem x "aeiou") xs

test = map ((\x -> if x == 3 then (negate x) else x))[1..10]

test' = map (\x -> bool x (negate x) (x == 3)) [1..10]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x : xs) = if f x then x : (myFilter f xs) else myFilter f xs


myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' f (x : xs)
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs

multiplesOf :: (Integral a) => a -> [a] -> [a]
multiplesOf x xs = filter (\y -> (==) (rem y x) 0) xs

numMultiples :: (Integral a) => a -> [a] -> Int
numMultiples x = length . (multiplesOf x)


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([],[])
myUnzip ((x, y) : left) = 
    (x : (fst (myUnzip left)), y : (snd (myUnzip left))) 

zipWidth :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWidth _ [] _ = []
zipWidth _ _ [] = []
zipWidth f (x : xs) (y : ys) = f x y : zipWidth f xs ys


myZip' = zipWidth (,)


capitalise :: [Char] -> [Char]
capitalise [] = []
capitalise (x : xs) = toUpper x : xs


capitalise' :: [Char] -> [Char]
capitalise'[] = []
capitalise' (x : xs) = toUpper x : capitalise' xs

capitaliseAndReturn :: [Char] -> Char
capitaliseAndReturn = head . capitalise

caesar :: Int -> Char -> Char
caesar n x 
    | isLower x = ['a'..'z'] !! (((n + ord x) - ord 'a') `mod` 26) 
    | otherwise = ['A'..'Z'] !! (((n + ord x) - ord 'A') `mod` 26)

uncaesar n x = caesar (negate n) x


encrypt n = map (caesar n)


myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f xs = myOr (map f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y : ys) = (==) x y || myElem x ys


myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ (x : [])


squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : []) = x
myMaximumBy f (x : y : xs) = case f x y of
                                LT -> myMaximumBy f (y : xs)
                                GT -> myMaximumBy f (x : xs)
                                EQ -> myMaximumBy f (x : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : []) = x
myMinimumBy f (x : y : xs) = case f x y of
                                LT -> myMinimumBy f (x : xs)
                                GT -> myMinimumBy f (y : xs)
                                EQ -> myMinimumBy f (x : xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

