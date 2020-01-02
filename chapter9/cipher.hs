module Cipher where

import           Data.Char


caesar :: [Char] -> Int -> [Char]
caesar x n = map (shiftChar n) x

uncaesar :: [Char] -> Int -> [Char]
uncaesar x n = map (shiftChar (negate n)) x


--65 -> 90 A-Z
--97 -> 122 a-z
shiftChar :: Int -> Char -> Char
shiftChar n x | isUpper x = head (f ['A' .. 'Z'])
              | isLower x = head (f ['a' .. 'z'])
              | otherwise = x
 where
  f casedLetters = drop
    (mod (ord x + n - ord (head casedLetters)) (length casedLetters)) casedLetters

-- consider using flip
id' :: [Char] -> Int -> [Char]
id' x n = uncaesar (caesar x n) $ n

myAnd :: [Bool] -> Bool
myAnd []           = True
myAnd (False : _ ) = False
myAnd (True  : xs) = myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' []       = True
myAnd' (x : xs) = x && myAnd' xs



myOr :: [Bool] -> Bool
myOr []       = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f


myElem :: Eq a => a -> [a] -> Bool
myElem _ []       = False
myElem x (y : ys) = x == y || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = myAny (== x) ys


myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ x : []


squish :: [[a]] -> [a]
squish []       = []
squish (x : xs) = x ++ squish xs


-- squishMap' :: (a -> [b]) -> [a] -> [b]
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []       = error "Cannot get maximum of empty list"
myMaximumBy _ (x : []) = x
myMaximumBy f (x : xs) = case f x (head xs) of
  GT -> myMaximumBy f (x : tail xs)
  _  -> myMaximumBy f xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []       = error "Cannot get maximum of empty list"
myMinimumBy _ (x : []) = x
myMinimumBy f (x : xs) = case f x (head xs) of
  LT -> myMinimumBy f (x : tail xs)
  _  -> myMinimumBy f xs


myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
