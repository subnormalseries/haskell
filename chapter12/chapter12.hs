module Maybe where

import Data.Char
import Data.List
import Data.Maybe

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 x = if even x then Just (x + 2) else Nothing

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> ValidatePerson Person 
mkPerson name age  = mkPerson' (nameOkay name) (ageOkay age) 

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right validatedName) (Right validatedAge) = Right (Person validatedName validatedAge)
mkPerson' (Right _) (Left invalidAge) = Left invalidAge
mkPerson' (Left invalidName) (Right _) = Left invalidName
mkPerson' (Left invalidName) (Left invalidAge) = Left (invalidName ++ invalidAge)

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

djali = mkPerson "Djali" 5
type ValidatePerson a = Either [PersonInvalid] a -- partially applied Either datatype

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age 
    | age >= 0 = Right age
    | otherwise = Left (AgeTooLow : []) 


nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
    | name /= "" = Right name 
    | otherwise = Left (NameEmpty : [])


r :: a -> f a
r = undefined

notThe :: String -> Maybe String
notThe word = case (==) "the" ([toLower c | c <- word]) of
    True -> Nothing
    False -> Just word


replaceThe :: String -> [String] 
replaceThe words = map f ((map (notThe)  (splitStringOnChar ' ' words)))
    where f Nothing = "a"
          f (Just word) = word


splitStringOnChar :: Char -> String -> [String]
splitStringOnChar _ "" = []
splitStringOnChar c xs = (takeWhile (\x -> x /= c) xs) : (splitStringOnChar c (dropWhile (== c) (dropWhile (\x -> x /= c) xs)))

concatenateStrings :: [String] -> String
concatenateStrings ls = init $ foldr (\a b -> a ++ " " ++ b) "" ls

splitWords :: String -> [String]
splitWords string =
    go string []
        where
          go [] acc = reverse acc
          go (x:xs) [] = go xs [[x]]
          go (x:xs) ht@(str@(y:_):ts) =
              if isLetter x == isLetter y
              then go xs ((str ++ [x]):ts)
              else go xs ([x]:ht)


zipAdj :: [a] -> [(a, a)] 
zipAdj x = zip x $ tail x

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

isConsonant :: Char -> Bool
isConsonant = flip elem "bcdfghjklmnpqrstvwxyz"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel words = foldr f 0 (zipAdj (splitStringOnChar ' ' words))
                where f (first, secondHead: _) count = 
                        if ((==) "the" first && isVowel secondHead) 
                            then count + 1 
                        else count
                    
countCharsMatchRule :: (Char -> Bool) -> String -> Integer
countCharsMatchRule f = foldr (\a b -> if f a then b + 1 else b) 0


newtype Word' = Word' String deriving (Eq, Show)

numVowels :: String -> Integer
numVowels = countCharsMatchRule isVowel 

numConsonants :: String -> Integer
numConsonants = countCharsMatchRule isConsonant 

mkWord :: String -> Maybe Word'
mkWord word 
    | (numVowels word) > (numConsonants word) = Nothing
    | otherwise = Just (Word' word)

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n 
    | n < 0 = Nothing
    | n == 0 = Just Zero
    | otherwise = insertSucc $ (integerToNat (n - 1))

insertSucc :: Maybe Nat -> Maybe Nat
insertSucc (Just x') = Just (Succ x')
insertSucc Nothing = Nothing


isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' (Just _) = True

isNothing' :: Maybe a -> Bool
isNothing' = not . isJust'

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMayybee :: a -> Maybe a -> a
fromMayybee a Nothing = a
fromMayybee _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing : xs) = catMaybes xs
catMaybes' ((Just x) : xs) = x : catMaybes xs

-- Favour folds over direct recursion
catMaybes'' :: [Maybe a] -> [a]
catMaybes'' = foldr f []
        where f Nothing b = b 
              f (Just a) b = a : b

lefts :: [Either a b] -> [a]
lefts = foldr f []
    where f (Left a) acc = a : acc
          f _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
    where f (Right b) acc = b : acc
          f _ acc = acc


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts eithers, rights' eithers)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' _ g (Right b) = g b
either' f _ (Left a) = f a


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b) 

--Unfolds
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f a 

myIterate' :: (a -> a) -> a -> [a]
myIterate' f a = unfoldr (\x -> Just(x, f x)) a

-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- myUnfoldr f b =  


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case (f b) of
   Nothing -> []
   Just (x, y) -> x : myUnfoldr f (y)

treeUnfoldr :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
treeUnfoldr f a = case (f a) of
    Nothing -> Leaf
    Just (x, y, z) -> Node (treeUnfoldr f x) y (treeUnfoldr f z)


treeBuild :: Integer -> BinaryTree Integer
treeBuild x = treeUnfoldr f 0
    where f n = if (n < x) then Just (n + 1, n, n + 1) else Nothing