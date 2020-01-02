module ListStuff where


import           Data.Bool
import           Data.Char

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x : []) = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []       = Nothing
safeHead (x : xs) = Just x

-- This is a much more generic enumFromThen than the exercises.
enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo' x y | (x > y)   = []
                | x == y    = x : []
                | otherwise = x : enumFromTo' (succ x) y


take' :: Int -> [a] -> [a]
take' 0 _        = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs       = xs
drop' n (x : xs) = drop' (n - 1) xs


splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, tail (drop (n - 1) xs))


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x : xs) | f x       = x : takeWhile' f xs
                      | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f (x : xs) | f x       = dropWhile' f xs
                      | otherwise = x : xs


map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

negateThree = map (\x -> if x == 3 then negate x else x) [1 .. 10]

negateThree' = map (\x -> bool x (negate x) (x == 3)) [1 .. 10]

itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) | f x       = x : filter' f xs
                   | otherwise = filter' f xs


multiplesOfThree = filter (\x -> rem x 3 == 0)
numMuliplesOfThree = length . multiplesOfThree

removeArticles xs = filter (\x -> not (elem x ["an", "a", "the"])) $ words xs


zip' :: [a] -> [b] -> [(a, b)]
zip' []       _        = []
zip' _        []       = []
zip' (x : xs) (y : ys) = (,) x y : zip' xs ys

zipWidth' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWidth' _ []       _        = []
zipWidth' _ _        []       = []
zipWidth' f (x : xs) (y : ys) = f x y : zipWidth' f xs ys


zip'' :: [a] -> [b] -> [(a, b)]
zip'' xs ys = zipWidth' (\a b -> (a, b)) xs ys

removeLowercase :: [Char] -> [Char]
removeLowercase = filter isUpper

capitaliseFirstCharacter :: [Char] -> [Char]
capitaliseFirstCharacter []       = []
capitaliseFirstCharacter (x : xs) = toUpper x : xs

recursiveCapitaliseString :: [Char] -> [Char]
recursiveCapitaliseString []       = []
recursiveCapitaliseString (x : xs) = toUpper x : recursiveCapitaliseString xs

capitaliseFirstCharacterAndReturnIt :: [Char] -> Char
capitaliseFirstCharacterAndReturnIt = head . capitaliseFirstCharacter








-- This is a partial function because it doesn't take care of every case.
-- E.g. what happens when the list is empty. What is the first element of the 
-- empty list? This is where the Imperative language would just return null
-- to mean the absence of an element. But in functional languages we use
-- the Option type or Maybe type in Haskell
myHead :: [a] -> a
myHead (x : _) = x


-- I would have thought that the compiler would prevent this function
-- Only partially matching on the Maybe type. This is a non exhaustive list of possible 
-- outcomes.
myHead' :: [a] -> Maybe a
myHead' (x : _) = Just x


myHead'' :: [a] -> Maybe a
myHead'' []      = Nothing
myHead'' (x : _) = Just x

efBool :: Bool -> [Bool]
efBool False = [False, True]
efBool True  = [True]

-- This was my attempt at the enumFromThen function on the Enum Bool typeclass
-- It doesn't feel right to exhaustively pattern match for this sum type.
-- This approach won't work for other types so would probably benefit a more
-- general solution using the rest of the Enum typeclass


-- I was getting confused with enumFromTo and enumFromThen hence the confusing function definition
-- enumFromTo does depend on the ordering otherwise it returns an empty list.

eftBool :: Bool -> Bool -> [Bool]
eftBool True  False = []
eftBool False True  = [False, True]
eftBool True  True  = [True]
eftBool False False = [False]

-- A generic implementation of enumFromTo 
-- Used fromEnum to convert the Enum Type to a type that has ordering without
-- explicitly declaring it in the type signature. 
eft :: Enum a => a -> a -> [a]
eft x y | fromEnum x > fromEnum y  = []
        | fromEnum x == fromEnum y = [x]
        | otherwise                = x : eft (succ x) y



eftBool' :: Bool -> Bool -> [Bool]
eftBool' = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft



myTake :: Int -> [a] -> [a]
myTake _ []       = []
myTake 0 _        = []
myTake n (x : xs) = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop _ []       = []
myDrop 0 xs       = xs
myDrop n (x : xs) = myDrop (n - 1) xs


splitAt'' :: Int -> [a] -> ([a], [a])
splitAt'' n xs = (,) (take n xs) (drop n xs)

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' _ [] = []
takeWhile'' f (x : xs) | f x       = x : takeWhile'' f xs
                       | otherwise = []

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' _ [] = []
dropWhile'' f (x : xs) | f x       = dropWhile'' f xs
                       | otherwise = x : xs



-- I had problems here due to using "" instead of ''
myWords :: [Char] -> [[Char]]
myWords [] = []
myWords (' ' : xs) = myWords xs -- This is the important pattern
-- If the first char is a space just skip over it. 
myWords xs = (takeWhile (\y -> y /= ' ') xs) : myWords (dropWhile (/= ' ') xs)

-- genericise this solution
splitAt''' :: (a -> Bool) -> [a] -> [[a]]
splitAt''' _ []       = []
splitAt''' f (x : xs) = if f x
  then takeWhile f (x : xs) : splitAt''' f (dropWhile f (x : xs))
  else splitAt''' f xs

myWords' = splitAt''' (/= ' ')


-- List Comprehensions
-- A means of generating a new list from a list or lists.
-- Similar definition to set comprehensions which I have seen in maths


mySqr = [ x ^ 2 | x <- [1 .. 10] ]

a = [ x | x <- mySqr, even x ]

b = [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

c = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]


removeLowercase' :: String -> String
removeLowercase' x = [ y | y <- x, isUpper y ]

-- This can clearly be generalised a little more by abstracting out the filter
removeChars :: (Char -> Bool) -> String -> String
removeChars f x = [ y | y <- x, f y ]


--acronym generator
-- takes in a string and returns an acronym for it.
myCube = [ y ^ 3 | y <- [1 .. 10] ]

tuple = [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]


-- A list is either an empty list or a cons cell
-- The cons operator is :



{- About the list type.
The list type has a type constructor of [] which is a parameteric type constructor
It has 2 data constructors AKA value constructors
The first being []
The second being a : [a]
: is the cons operator which just 
-}


map'' :: (a -> b) -> [a] -> [b]
map'' _ []       = []
map'' f (x : xs) = f x : map'' f xs


-- take 1 $ map (+1) [undefined, 2, 3] -- bottom
-- take 1 $ map (+1) [1, undefined, 3] -- fine
-- take 2 $ map (+1) [1, undefined, 3] -- bottom

itIsMystery' :: String -> [Bool]
itIsMystery' xs = map (\x -> x `elem` "aeiou") xs

-- map (\z -> if z == 3 then negate 3 else z) [1..10]

-- write this interms of bool


-- bool :: a -> a -> Bool -> a
mapIfThenElse :: (Num a, Eq a) => [a] -> [a]
mapIfThenElse xs = map (\z -> bool z (negate z) (z == 3)) xs


mapIfThenElsePointFree = map (\z -> bool z (negate z) (z == 3))


multiplesOfThree' :: [Int] -> [Int]
multiplesOfThree' = filter (\z -> (==) (mod z 3) 0)

myFilter' :: String -> [String]
myFilter' xs = filter (\z -> z /= "the" && z /= "a" && z /= "an") (words xs)



zip''' :: [a] -> [b] -> [(a, b)]
zip''' []       _        = []
zip''' _        []       = []
zip''' (x : xs) (y : ys) = (,) x y : zip''' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = (,) [] []
unzip' (head : rest) =
  ((fst head) : (fst $ unzip' rest), (snd head) : (snd $ unzip' rest))

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' _ []       _        = []
zipWith'' _ _        []       = []
zipWith'' f (x : xs) (y : ys) = f x y : zipWith'' f xs ys

zip'''' :: [a] -> [b] -> [(a, b)]
zip'''' = zipWith'' (\x y -> (x, y))



-- 9.12

onlyUppercase :: String -> String
onlyUppercase = filter isUpper

capitaliseFirstCharacter' :: String -> String
capitaliseFirstCharacter' []       = []
capitaliseFirstCharacter' "woot"   = "WOOT"
capitaliseFirstCharacter' (x : xs) = toUpper x : xs


capitaliseFirstCharacterAndReturnIt' :: String -> Char
capitaliseFirstCharacterAndReturnIt' = toUpper . head


-- Ciphers























