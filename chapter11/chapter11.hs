module AlgebraicDataTypes where

import Data.Char

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge
a :: PugType
a = PugData

b :: HuskyType a
b = HuskyData

c :: (Num a) => DogueDeBordeaux [a]
c = DogueDeBordeaux [1, 2, 3] -- DogueDeBordeaux [Integer]


data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
d :: Doggies String
d = Husky "James"

e :: Doggies Int
e = Mastiff 8

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show) 

type Size = Integer

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 50

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False


isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False


areCars :: [Vehicle] -> [Bool]
areCars = map isCar


getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

data Example = MakeExample Int

data Person = MkPerson String Int deriving (Eq, Show)
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson name _) = name


data Person2 = 
        Test { name :: String
               , age :: Int } deriving (Eq, Show)


--data Fiction = Fiction deriving Show
--data NonFiction = NonFiction deriving Show
--data BookType =   FictionBook Fiction
--                | NonFictionBook NonFiction 
--                deriving Show

type AuthorName = String -- type alias
-- data Author = Author (AuthorName, BookType)
data Author = Fiction AuthorName | NonFiction AuthorName deriving (Eq, Show)



--data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String
-- data Garden = Garden Gardener FlowerType deriving Show
data Garden = Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener deriving Show

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b =
    RecordProduct { pfirst :: a, psecond :: b} deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig
newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep) 

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo) 

bess = (CowInfo "Bess" 4)
bess' = First bess :: Animal'
e' = Second (SheepInfo "Elmer" 5 5)
elmer = Second e' :: Animal'

sheep = SheepInfo "Baaaaaa" 5 5



trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

-- data Twitter = Twitter deriving (Eq, Show)
-- data AskFm = AskFm deriving (Eq, Show)
-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

data SocialNetwork = Twitter | AskFm deriving (Eq, Show)


myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

data OperatingSystem = GnuPlusLinux 
                     | OpenBSDPlusNevermindJustBSDStill 
                     | Mac
                     | Windows deriving (Eq, Show)

data ProgLang = Haskell | Agda | Idris | Purescript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang} deriving (Eq, Show)

programmer :: Programmer 
programmer = Programmer { os = Windows, lang = Haskell }


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages] 

data ThereYet = 
    There Float Int Bool
    deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yes :: ThereYet
yes = notQuite False 

newtype Acres = Acres Int deriving Show
newtype Name' = Name' String deriving Show
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show
data Farmer = Farmer Name' Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec { name' :: Name', acres :: Acres, farmerType :: FarmerType }

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of 
                            DairyFarmer -> True
                            _ -> False
                            


data Quantum = Yes | No | Both deriving Show

functionType :: Bool -> Quantum
functionType True = Yes
functionType False = Yes

functionType1 :: Bool -> Quantum
functionType1 True = Yes
functionType1 False = No

functionType2 :: Bool -> Quantum
functionType2 True = Yes
functionType2 False = Both

data Silly a b c d = MkSilly a b c d deriving Show

data List a = Nil | Cons a (List a) deriving Show -- Note that the Cons data constructor is prefix
listExample :: List Integer
listExample = Cons 5 Nil

listExample2 = Nil 


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
    | b == a = Node left a right   
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

-- Build up our binary tree structure
t1 = insert' 0 Leaf
t2 = insert' 2 t1 
t3 = insert' 1 t2
t4 = insert' (-1) t3

mapTree :: (s -> t) -> BinaryTree s -> BinaryTree t
mapTree _ Leaf = Leaf
mapTree f (Node left s right) = (Node (mapTree f left) (f s) (mapTree f right))


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)


mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: String
mapOkay = if mapTree (+1) testTree == mapExpected then "Correct"
          else "Incorrect"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ ([a]) ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorderAnswer = [2, 1, 3]
inorderAnswer = [1, 2, 3]
postorderAnswer = [1, 3, 2]


foldTree :: (a ->b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right

lowercaseLetters :: String
lowercaseLetters = ['a'..'z']

uppercaseLetters :: String
uppercaseLetters = map toUpper lowercaseLetters


cipher :: (Char, Char) -> Char
cipher (key, char) | isLower char = lowercaseLetters !! (mod ((ord key) - 2 * (ord (head lowercaseLetters)) + (ord char)) 26)
                | isUpper char = uppercaseLetters !! (mod ((ord key) - 2 * (ord (head uppercaseLetters)) + (ord char)) 26)
                | otherwise = char



repeatString :: String -> String
repeatString = concat . repeat

vigenereCipher :: String -> String -> [Char]
vigenereCipher key word = map cipher $ zip (filter isAlpha word) (repeatString key)


f :: Show a => (a, b) -> IO (a, b)
f t@(a, b) = do
    print a 
    return t
-- This uses @ pattern matching. The @ allows a binding to match the whole of 
-- pattern rather than on a data constructor like general pattern matching
-- It works in conjunction of pattern matching
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x : _) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf(x : xs) ys = (elem x ys) && isSubseqOf xs ys

capitalizeWords :: String -> (String, String)
capitalizeWords [] = ("","") -- not sure about this
capitalizeWords word@(x : xs) = (word, ((toUpper x) : xs))

splitStringOnChar :: Char -> String -> [String]
splitStringOnChar _ "" = []
splitStringOnChar c xs = (takeWhile (\x -> x /= c) xs) : (splitStringOnChar c (dropWhile (== c) (dropWhile (\x -> x /= c) xs)))

capitalizeWords' :: String -> [(String, String)]
capitalizeWords' word = map capitalizeWords (splitStringOnChar ' ' word)


capitalizeWord :: String -> String
capitalizeWord (x : xs) = (toUpper x) : xs

-- This is not quite right as it doesn't account for the first space after a full stop. 
-- Carrying on anyway.
capitalizeParagraph :: String -> String 
capitalizeParagraph paragraph = concat $ map capitalizeWord $ splitStringOnChar '.' paragraph 

-- Phone Exercises
data DaPhone = DaPhone [String]
phone :: DaPhone
phone = DaPhone ["1", "2abc", "3def", "4ghi", "5jkl", "6mno", "7pqrs", "8tuv", "9wxyz", "*^", "0+-", "#.,"]

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya", 
         "U 1st haha", 
         "Lolok. Have u ever tasted alcohol", 
         "Lol ya", 
         "Wow ur cool haha. Ur turn", 
         "Ok. Do u think I am pretty Lol", 
         "Lol ya", 
         "Just making sure rofl ur turn"]

type Digit = Char
type Presses = Int
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined   
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined




