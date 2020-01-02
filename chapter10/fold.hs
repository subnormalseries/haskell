import           Data.Time
import           Data.List

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 34
  , DbString "Hello, World!"
  , DbDate (UTCTime (fromGregorian 1921 5 2) (secondsToDiffTime 34123))
  ]

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' [] = []
filterDbDate' ((DbDate utcTime) : xs) = utcTime : (filterDbDate' xs)
filterDbDate' (_ : xs) = filterDbDate' xs


filterDbDate'' :: [DatabaseItem] -> [UTCTime]
filterDbDate'' = foldr f []
                  where f (DbDate time) b = time : b
                        f _ b = b

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' = foldr f []
                  where f (DbNumber number) b = number : b
                        f _ b = b

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' = last . sort . filterDbDate'

sumDb' :: [DatabaseItem] -> Integer
sumDb' = sum . filterDbNumber'

avgDb' :: [DatabaseItem] -> Double
avgDb' items = fromIntegral(sumDb' items) / fromIntegral(length numbers)
                where numbers = filterDbNumber' items

fibs'' :: [Integer]
fibs'' = takeWhile (< 100) $ 1 : scanl (+) 1 fibs''

factorial' :: [Integer]
factorial' = take 10 $ scanl (*) 1 [1..]




stops = "pbtdkg"
vowels = "aeiou"

words' :: [Char] -> [Char] -> [(Char,Char,Char)]
words' stop vowel =  concat (concatMap (\s1->
                            map (\v->
                            map (\s2 -> (s1, v, s2)) stop) vowel) stop)

                    
words'' :: [(Char, Char, Char)]
words'' = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]


onlyP = takeWhile (\(a, _, _) -> a == 'p') words''

seekritFunc x = 
    (/) (fromIntegral (sum (map length (words x)))) (fromIntegral (length (words x)))




myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> (f a || b)) False 


myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> (==) a x|| b) False


myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)  

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a : []) ++ b) []


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicate = foldr (\a b -> if (predicate a) then (a : []) ++ b else b) []


myFilter' predicate = foldr f []
                      where f a b 
                              | predicate a = a : b 
                              | otherwise = b


squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> (f a) ++ b) []


squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 


-- myMaximumBy :: (a -> a -> Ordering)
--             -> [a]
--             -> a
-- myMaximumBy order = foldr (
--       \a b -> case order a (head b) of
--         GT -> a : b
--         EQ -> a : b
--         LT -> b) [] 
myMaximumBy order xs = foldr1 (\a b -> if order a b == LT then b else a) xs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
 where
  f (DbDate time) b = time : b
  f _             b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
 where
  f (DbNumber x) b = [x]
  f _            b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = last . sort . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs =
  fromIntegral ((sumDb xs) `div` fromIntegral (length (filterDbNumber xs)))


-- Recursive function definition.

fibs = takeWhile (\x -> x < 100) (1 : scanl (+) 1 fibs)

fibs' = take 20 $ (1 : scanl (+) 1 fibs')

-- fibsN n = fibs' !! n

factorial = take 10 $ scanl (*) 1 [2 ..]
-- 
-- 
-- stops = "pbtdkg"
-- vowels = "aeiou"
-- 
-- combinations :: [Char] -> [Char] -> [(Char, Char, Char)]
-- combinations ss vs = [ (s1, v, s2) | s1 <- ss, v <- vs, s2 <- ss ]
-- 
-- combinationsBeginWithP ss vs = takeWhile (\x -> f x) $ combinations ss vs
--   where f (a, _, _) = a == 'p'
-- 
-- -- seekritFunc x =
-- --   div (sum (map (fromIntegral (length (words x))))) (5.0 :: Double)
-- --                     --(fromIntegral (length (words x))) -- average length of word in a string.
-- 
-- 
-- myAnd :: [Bool] -> Bool
-- myAnd []       = True
-- myAnd (x : xs) = if (not x) then x else myAnd xs
-- 
-- myAnd' :: [Bool] -> Bool
-- myAnd' = foldr (&&) True
-- 
-- 
-- myOr :: [Bool] -> Bool
-- myOr []       = False
-- myOr (x : xs) = (\a -> if a then a else myOr xs) x
-- 
-- 
-- -- Not point free. To be honest I don't think I visualise functions in this way
-- myOr' :: [Bool] -> Bool
-- myOr' = foldr (\a b -> if a then a else b) False
-- 
-- myOr'' :: [Bool] -> Bool
-- myOr'' = foldr (||) False
-- 
-- 
-- -- Any written in a non point free style using folds
-- myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f xs = foldr (\a b -> if f a == True then True else b) False xs
-- 
-- 
-- -- In a partial point free style
-- -- The folding function isn't point free
-- myAny' :: (a -> Bool) -> [a] -> Bool
-- myAny' f = foldr (\a b -> f a || b) False
-- 
-- myElem :: Eq a => a -> [a] -> Bool
-- myElem x = foldr (\a acc -> if a == x then True else acc) False
-- 
-- myElem' x = foldr (\a acc -> a == x || acc) False
-- 
-- myReverse xs = foldl' (flip (:)) [] xs
-- 
-- myMap :: (a -> b) -> [a] -> [b]
-- myMap f []       = []
-- myMap f (x : xs) = f x : (foldr (\a b -> (:) (f a) b) [] xs)
-- 
-- myMap' :: (a -> b) -> [a] -> [b]
-- myMap' f = foldr (\a b -> (:) (f a) b) []
-- 
-- 
-- myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter f []       = []
-- myFilter f (x : xs) = case f x of
--   True  -> x : myFilter f xs
--   False -> myFilter f xs
-- 
-- 
-- myFilter' :: (a -> Bool) -> [a] -> [a]
-- myFilter' f [] = []
-- myFilter' f (x : xs) | f x  = x : myFilter' f xs
--                      | otherwise = myFilter' f xs
-- 
-- myFilter'' f []       = []
-- myFilter'' f (x : xs) = if f x then x : myFilter'' f xs else myFilter'' f xs
-- 
-- 
-- pizza = foldr (\a b -> (take 3 a) ++ b) "" ["Pizza", "Apple", "Banana"]
-- 

-- 