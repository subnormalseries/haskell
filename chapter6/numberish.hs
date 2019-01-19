import           Data.List

class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a

newtype Age = Age' Integer deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age' n
    toNumber (Age' n) = n
    defaultNumber = Age' 65

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
    fromNumber x = Year x
    toNumber (Year x) = x
    defaultNumber = Year 1988



sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
 where
  integerA  = toNumber a
  integerA' = toNumber a'
  summed    = integerA + integerA'



addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y = if x > 1 then x + y else x

add :: Int -> Int -> Int
add x y = (+) x y

addWeird' :: Int -> Int -> Int
addWeird' x y = if x > 1 then x + y else x

check :: Int -> Int -> Bool
check x y = (==) x y

data Fraction = Fraction Integer Integer deriving (Show, Eq)





data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


data Mood = Blah | Woot deriving (Show, Eq, Ord)

settleDown x = if x == Woot then Blah else x

-- aliases
type Subject = String
type Verb = String
type Object = String

-- Subject -> Verb -> Object -> Sentence is the type signature of the Sentence data constructor.
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)


---Won't print because :t s1 is Object -> Sentence
-- And by default  the type constructor (->) doesn't have an instance of the Show typeclass.
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"



--datetype definition
data Rocks = Rocks String deriving (Show, Eq)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

truth = Papu (Rocks "James") $ Yeah True

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'


i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

-- freud' :: a -> a
-- freud' x = x

freud' :: Int -> Int
freud' x = x



jung :: [Int] -> Int
jung xs = head $ sort xs



mySort :: [Char] -> [Char]
mySort = sort

signifier :: String -> Char
signifier xs = head $ mySort xs




chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x a = (+) (f a) (fromInteger x) -- Order matters when it comes to the compiler


bindExp :: Integer -> String
bindExp x =
  let y = 5 in "the integer was: " ++ show x ++ " and y was: " ++ show y


bindExp' :: Integer -> String
bindExp' x =
  let y = 5
  in  let z = y + x
      in  "the integer was: "
          ++ show x
          ++ " and y was "
          ++ show y
          ++ " and z was "
          ++ show z


-- Shadowing
bindExp'' :: Integer -> String
bindExp'' x =
  let x = 10
      y = 5
  in  "the integer was " ++ show x ++ " and y was " ++ show y


mth x y z = x * y * z

mth' x y = \z -> x * y * z

mth'' x = \y -> \z -> x * y * z

mth2 = \x -> \y -> \z -> x * y * z


addOne = \x -> x + 1

addOneIfOdd n = if mod n 2 == 1 then addOne n else n

addOneIfOdd' n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1


addFive x y = (if x > y then y else x) + 5
addFive' = \y -> \x -> (+ 5) $ (if x > y then y else x)


mflip f = \x -> \y -> f y x
mflip' f x y = f y x

