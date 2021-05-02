{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Test.QuickCheck
import           Text.Show.Functions
import           GHC.Generics
import           Data.List
import           Data.Char
data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial


instance Arbitrary Trivial where
    arbitrary = trivialGen


data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenBool :: Gen (Identity Bool)
identityGenBool = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = First a | Second b deriving (Eq, Show)
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return (First a), return (Second b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGenEqual

--CoArbitrary
{-
CoArbitrary is a counterpart to Arbitrary that enables the generation of funtions fitting a particular type
-}

-- arbitrary :: Arbitrary a => Gen a
-- coarbi

integerGen :: Gen Integer
integerGen = elements [1 ..]

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'


trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

half :: Fractional a => a -> a
half x = (/) x 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half
prop_doubleHalfIdentity :: Property
prop_doubleHalfIdentity =
  forAll (arbitrary :: Gen Double) (\n -> halfIdentity n == id n)

--checks whether list is ordered
listOrdered :: (Ord a) => [a] -> Bool
listOrdered []         = True
listOrdered y@(x : xs) = all (\(x, y) -> x <= y) $ zip y xs

prop_sortedListOrdered :: Property
prop_sortedListOrdered =
  forAll (arbitrary :: Gen [Integer]) (\xs -> listOrdered (sort xs) == True)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = (x + y) + z == x + (y + z)

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = (x + y) == (y + x)

multiplicationAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multiplicationAssociative x y z = x * (y * z) == (x * y) * z

multiplicationCommutative :: (Eq a, Num a) => a -> a -> Bool
multiplicationCommutative x y = (x * y) == (y * x)

quotRem' :: Integral a => a -> a -> Bool
quotRem' x y = (quot x y) * y + (rem x y) == x

prop_quotRem :: Property
prop_quotRem = forAll (nonZeroInteger :: Gen Integer) quotRem'


divMod' :: Integral a => a -> a -> Bool
divMod' x y = (div x y) * y + (mod x y) == x

prop_divMod :: Property
prop_divMod = forAll nonZeroInteger divMod'
-- Writing my own generator for integer that is for non zero integers
-- suchThat is a function in the QuickCheck package that allows us to filter our
-- generated values
nonZeroInteger :: Gen Integer
nonZeroInteger = suchThat (arbitrary :: Gen Integer) (/= 0)

exponentAssoc :: Integer -> Integer -> Integer -> Bool
exponentAssoc = (\a b c -> ((a ^ b) ^ c) == (a ^ (b ^ c)))

reverseId :: Eq a => [a] -> Bool
reverseId = (\a -> ((reverse . reverse) a) == (id a))


-- write a property for the defintion of $
-- $
-- This example invloves randomising the function and it's input.
-- Limiting myself to Integer -> Integer for the function I need
-- a way of randomly generating functions.
-- This is teased in the Coarbitrary section but to no real depth
-- import Text.Show.Functions in order for the source code to compile
-- This package provides a Show typeclass instance for functions
prop_dollar :: (Integer -> Integer) -> Integer -> Bool
prop_dollar f x = f x == (f $ x)


prop_compose :: (Eq a, Eq b, Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_compose f g x = (f . g) x == f (g (x))

prop_fold :: Eq a => [a] -> [a] -> Bool
prop_fold xs ys = (foldr (:) ys xs) == (++) xs ys

prop_fold' :: Eq a => [[a]] -> Bool
prop_fold' xs = (foldr (++) [] xs) == concat xs

prop_lengthTake :: Int -> [a] -> Bool
prop_lengthTake n xs = length (take n xs) == n

prop_readShow :: Integer -> Bool
prop_readShow x = (read . show) x == x

square n = n * n


--prop_squareIdentity :: Integer -> Bool
-- prop_squareIdentity n = (square . sqrt) n == n

twice f = f . f
fourTimes = twice . twice

twiceProperty :: Property
twiceProperty = forAll
  (arbitrary :: Gen String)
  (\x ->
    (capitalizeWord x == twice capitalizeWord x)
      && (capitalizeWord x == fourTimes capitalizeWord x)
  )

sortProperty :: Property
sortProperty = forAll
  (arbitrary :: Gen [Char])
  (\c -> (sort c == twice sort c) && (sort c == fourTimes sort c))

main :: IO ()
main = do
  sample pairGenIntString
  sample (sumGenEqual :: Gen (Sum String Bool))
  sample trueGen
  quickCheck prop_doubleHalfIdentity
  quickCheck prop_sortedListOrdered
  quickCheck (plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (plusCommutative :: Integer -> Integer -> Bool)
  quickCheck
    (multiplicationAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (multiplicationCommutative :: Integer -> Integer -> Bool)
  quickCheck (prop_quotRem)
  quickCheck (prop_divMod)
  -- This is checking that exponation is not associative.
  -- The given result is that (0^0)^0 = 1 whereas 0^(0^0) = 0
  quickCheck (forAll (arbitrary :: Gen Integer) exponentAssoc)
  quickCheck (reverseId :: String -> Bool)
  verboseCheck (prop_dollar)
  quickCheck
    (prop_compose :: (String -> Integer)
      -> (Integer -> String)
      -> Integer
      -> Bool
    )
  quickCheck (prop_fold :: [Integer] -> [Integer] -> Bool)
  quickCheck (prop_fold' :: [[Integer]] -> Bool)
  quickCheck (prop_lengthTake :: Int -> [String] -> Bool)
  quickCheck (prop_readShow)
  quickCheck (twiceProperty)
  quickCheck (sortProperty)
  quickCheck (withMaxSuccess 1000 prop_caesar)
  quickCheck (withMaxSuccess 1000 prop_vigenere)

capitalizeWord :: String -> String
capitalizeWord = map toUpper

data Fool = Fulse | Frue deriving (Eq, Show)
instance Arbitrary (Fool) where
  arbitrary = frequency [(2, return Fulse), (1, return Frue)]

type Key = Int

caesar :: Key -> [Char] -> [Char]
caesar n = map $ (chr . ordAfterShift n)

ordAfterShift :: Key -> Char -> Int
ordAfterShift n x | isUpper x = go 'A'
                  | isLower x = go 'a'
                  | otherwise = ord x
 where
  go character =
    ord character + mod ((ord x `mod` ord character) + (n `mod` 26)) 26

uncaesar :: Key -> [Char] -> [Char]
uncaesar n x = caesar (negate n) x

lowercaseLetters :: String
lowercaseLetters = ['a' .. 'z']

uppercaseLetters :: String
uppercaseLetters = map toUpper lowercaseLetters

toLowercaseAlphabetIndex :: Char -> Int
toLowercaseAlphabetIndex c = ord c - (ord (head lowercaseLetters))

toUppercaseAlphabetIndex :: Char -> Int
toUppercaseAlphabetIndex c = ord c - (ord (head uppercaseLetters))

numAlphabetLetters :: Int
numAlphabetLetters = 26

encode :: KeyChar -> Char -> Char
encode key char
  | isLower char
  = lowercaseLetters
    !! (mod (toLowercaseAlphabetIndex key + toLowercaseAlphabetIndex char)
            numAlphabetLetters
       )
  | isUpper char
  = uppercaseLetters
    !! (mod (toUppercaseAlphabetIndex key + toUppercaseAlphabetIndex char)
            numAlphabetLetters
       )
  | otherwise
  = char

decode :: KeyChar -> Char -> Char
decode key char
  | isLower char = encode
    (  lowercaseLetters
    !! (mod ((numAlphabetLetters) - (toLowercaseAlphabetIndex key))
            numAlphabetLetters
       )
    )
    char
  | isUpper char = encode
    (  uppercaseLetters
    !! (mod ((numAlphabetLetters) - (toUppercaseAlphabetIndex key))
            numAlphabetLetters
       )
    )
    char
  | otherwise = char

prop_caesar :: Key -> String -> Bool
prop_caesar key word = uncaesar key (caesar key word) == word

prop_vigenere :: KeyWord -> String -> Bool
prop_vigenere keyWord word =
  (vigenereCipher decode keyWord (vigenereCipher encode keyWord word)) == word

type KeyChar = Char
type KeyWord = [KeyChar]



vigenereCipher :: (KeyChar -> Char -> Char) -> KeyWord -> String -> [Char]
vigenereCipher _ _ [] = []
vigenereCipher f key word =
  zipWith f ((concat . repeat) key) (filter isAlpha word)

-- Problem here is that an empty character is a valid character and cycle doesn't work with
-- empty characters. Create a non empty string type using a smart constructor
-- cycle is not a total function. It is partial, it doesn't work on all inputs

